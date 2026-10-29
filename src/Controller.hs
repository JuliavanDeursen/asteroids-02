-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random


step :: Float -> GameState -> IO GameState
step dt gamestate
    | paused gamestate   = return gamestate
    | gameOver gamestate = return gamestate
    | otherwise  = return (updateGame dt gamestate)

updateGame :: Float -> GameState -> GameState
updateGame dt gamestate =
    let player'  = updatePlayer dt (player gamestate)
        bullets' = updateBullets dt (bullets gamestate)
        explosions' = updateExplosions dt (explosions gamestate)

        (as2, bullets2, ex2, scoreGain) = bulletHits bullets' (asteroids gamestate)
        asteroids' = map (updateAsteroid dt) as2

        -- collisions
        playerDead = playerHit player' asteroids'
    in
        gamestate { player     = player'
          , asteroids  = asteroids'
          , bullets    = bullets2
          , explosions = explosions' ++ ex2
          , score      = score gamestate + scoreGain
          , gameOver   = playerDead
          }

--update a players position during time
updatePlayer :: Float -> Player -> Player
updatePlayer dt p =
    let (x, y) = playerPosition p
        (vx, vy) = playerVelocity p

        friction = 0.99 --Factor to slow down the player
        newVx = vx * friction
        newVy = vy * friction

        newX = x + newVx * dt
        newY = y - newVy * dt

        wrapX = wrapCoord (-400) 400 newX
        wrapY = wrapCoord (-300) 300 newY
    in p { playerPosition = (wrapX, wrapY) }

--Helpfunction to wrap coordinates around the screen edges
wrapCoord :: Float -> Float -> Float -> Float
wrapCoord min max val
    | val < min = max
    | val > max = min
    | otherwise = val

-- updates an asteroids position during time 
updateAsteroid :: Float -> Asteroid -> Asteroid
updateAsteroid dt a =
    let (x, y) = asteroidPosition a
        (vx, vy) = asteroidVelocity a

        newX = x + vx * dt
        newY = y + vy * dt

        wrapX = wrapCoord (-400) 400 newX
        wrapY = wrapCoord (-300) 300 newY
    in a { asteroidPosition = (wrapX, wrapY) }

--map update bullet so the entire list get updated during time
updateBullets :: Float -> [Bullet] -> [Bullet]
updateBullets dt bullets =
    map (updateBullet dt) bullets

updateBullet :: Float -> Bullet -> Bullet
updateBullet dt b =
    let (x, y) = bulletPosition b
        (vx, vy) = bulletVelocity b
        newPosition = (x + vx * dt, y + vy * dt)
    in b { bulletPosition = newPosition }

--sets a fixed time for explosions, and updates them during time
explosionDuration :: Float
explosionDuration = 1.2

updateExplosions :: Float -> [Explosion] -> [Explosion]
updateExplosions dt explosions =
    let update e =
            e { explosionTime = explosionTime e + dt }
    in filter (\e -> explosionTime e < explosionDuration)
              (map update explosions)

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState

--Pause the game when the user presses 'p'
inputKey (EventKey (Char 'p') Down _ _) gstate =
    gstate { paused = not (paused gstate) }

--Left arrow key rotates the player to the left
inputKey (EventKey (SpecialKey KeyLeft) Down _ _) gstate =
    let p = player gstate
    in gstate { player = p { playerAngle = playerAngle p - 15 } }

--Right arrow key rotates the player to the right
inputKey (EventKey (SpecialKey KeyRight) Down _ _) gstate =
    let p = player gstate
    in gstate { player = p { playerAngle = playerAngle p + 15 } }

--Up arrow key accelerates the player in the direction they are facing
inputKey (EventKey (SpecialKey KeyUp) Down _ _) gstate =
    let p = player gstate
        angleRad = playerAngle p * (pi / 180)
        acceleration = 50
        (vx, vy) = playerVelocity p
        newVx = vx + acceleration * cos angleRad
        newVy = vy + acceleration * sin angleRad
    in gstate { player = p { playerVelocity = (newVx, newVy) } }

--Spacebar key fires a bullet
inputKey (EventKey (SpecialKey KeySpace) Down _ _) gstate =
    let p = player gstate
        angleRad = playerAngle p * (pi / 180)
        bulletSpeed = 300
        (px, py) = playerPosition p
        bulletVx = bulletSpeed * cos angleRad
        bulletVy = - (bulletSpeed * sin angleRad)
        newBullet = Bullet { bulletPosition = playerPosition p
                           , bulletVelocity = (bulletVx, bulletVy)
                           , bulletTime = 0
                           , bulletSize = 3 }
    in gstate { bullets = newBullet : bullets gstate }

inputKey _ gstate = gstate


--calculates if theres an collision
collision :: Point -> Point -> Float -> Bool 
collision (x1,y1) (x2,y2) size = 
  let dx = x1 -x2
      dy = y1-y2
  in sqrt(dx*dx +dy*dy) <size --size is botsing radius

--checks if a player was hit 
playerHit :: Player -> [Asteroid] -> Bool
playerHit player asteroids =
    any hit asteroids
  where
    hit a =
        collision
            (playerPosition player)
            (asteroidPosition a)
            (playerSize player + asteroidSize a)

bulletHits :: [Bullet] -> [Asteroid] -> ([Asteroid], [Bullet], [Explosion], Int)
bulletHits bullets asteroids =
    let isAsteroidHit a = 
            any (\b->
                collision 
                  (bulletPosition b)
                  (asteroidPosition a)
                  (asteroidSize a + bulletSize b) 
                ) bullets 
        isBulletHit b = 
            any (\a->
                collision 
                  (bulletPosition b)
                  (asteroidPosition a)
                  (asteroidSize a + bulletSize b) 
                ) asteroids

-- Filter out asteroids that were hit by bullets
        remainingAsteroids = filter (not . isAsteroidHit) asteroids
        hitAsteroids = filter isAsteroidHit asteroids

-- Filter out bullets that hit asteroids
        remainingBullets = filter (not . isBulletHit) bullets

        newExplosions =
            map (\a -> Explosion
                { explosionPosition = asteroidPosition a
                , explosionTime = 0.0
                }
                ) hitAsteroids

        scoreGain = length hitAsteroids * 100

    in (remainingAsteroids, remainingBullets, newExplosions, scoreGain)

--this functions makes the asteroids
--makeAsteroids :: GameState -> IO GameState
--makeAsteroids gamestate = 