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
        asteroids' = map (updateAsteroid dt) (asteroids gamestate)
        bullets' = updateBullets dt (bullets gamestate)
        explosions' = updateExplosions dt (explosions gamestate)

        -- collisions
        playerDead = playerHit player' asteroids'
        (as2, ex2, scoreGain) = bulletHits bullets' asteroids'
    in
        gamestate { player     = player'
          , asteroids  = as2
          , bullets    = bullets'
          , explosions = explosions' ++ ex2
          , score      = score gamestate + scoreGain
          , gameOver   = playerDead
          }

--update a players position during time
updatePlayer :: Float -> Player -> Player
updatePlayer dt p =
    let (x, y) = playerPosition p
        (vx, vy) = playerVelocity p
        newPosition = (x + vx * dt, y + vy * dt)
    in p { playerPosition = newPosition }

-- updates an asteroids position during time 
updateAsteroid :: Float -> Asteroid -> Asteroid
updateAsteroid dt a =
    let (x, y) = asteroidPosition a
        (vx, vy) = asteroidVelocity a
        newPosition = (x + vx * dt, y + vy * dt)
    in a { asteroidPosition = newPosition }

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

bulletHits :: [Bullet] -> [Asteroid] -> ([Asteroid], [Explosion], Int)
bulletHits bullets asteroids =
    let hit a = 
            any (\b->
                collision 
                  (bulletPosition b)
                  (asteroidPosition a)
                  (asteroidSize a + bulletSize b) 
                ) bullets 

        remainingAsteroids = filter (not . hit) asteroids
        hitAsteroids = filter hit asteroids

        newExplosions =
            map (\a -> Explosion
                { explosionPosition = asteroidPosition a
                , explosionTime = 1.2
                }
                ) hitAsteroids

        scoreGain = length hitAsteroids * 100

    in (remainingAsteroids, newExplosions, scoreGain)

--this functions makes the asteroids
--makeAsteroids :: GameState -> IO GameState
--makeAsteroids gamestate = 