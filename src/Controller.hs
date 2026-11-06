-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import HighScores

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | updates the gamestate over time
step :: Float -> GameState -> IO GameState
step dt gamestate
    | paused gamestate   = return gamestate
    | gameOver gamestate = return gamestate
    | otherwise = do
        let updateState = updateGame dt gamestate

        let numberDestroyed =
                length (asteroids gamestate) - length (asteroids updateState)

        let newHighScores =
                if gameOver updateState && not (gameOver gamestate)
                    then addHighScore (score updateState) (highScores gamestate)
                    else highScores gamestate

        let finalState = updateState
                { highScores = newHighScores }

        if gameOver updateState && not (gameOver gamestate)
            then saveHighScores newHighScores
            else return ()

        -- | calculates the amount of asteroids in the game based on the current score 
        let targetAsteroids = 5 + score finalState `div` 500

        -- | calculates the amount of new asteroids needed
        let asteroidsToSpawn =
                max 0 (targetAsteroids - length (asteroids finalState))

        if asteroidsToSpawn > 0 && not (gameOver finalState)
            then do
                gen <- newStdGen
                let newAsteroids =
                        makeRandomAsteroids asteroidsToSpawn (score finalState) gen

                return finalState
                    { asteroids = asteroids finalState ++ newAsteroids }
            else
                return finalState

-- | updates all movements and checks for collisions + score 
updateGame :: Float -> GameState -> GameState
updateGame dt gamestate =
    let player' = updatePlayer dt (keysPressed gamestate) (player gamestate)
        bullets' = updateBullets dt (bullets gamestate)
        explosions' = updateExplosions dt (explosions gamestate)

        (as2, bullets2, ex2, scoreGain) = bulletHits bullets' (asteroids gamestate)
        asteroids' = map (updateAsteroid dt (score gamestate + scoreGain) player') as2

        -- | collisions
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
updatePlayer :: Float -> [Key] -> Player -> Player
updatePlayer dt keys p =
    let (x, y) = playerPosition p
        (vx, vy) = playerVelocity p

        rotationSpeed = 180

        -- | determine rotation on pressed keys
        newAngle
            | SpecialKey KeyLeft `elem` keys =
                playerAngle p - rotationSpeed * dt
            | SpecialKey KeyRight `elem` keys =
                playerAngle p + rotationSpeed * dt
            | otherwise = playerAngle p

        -- Acceleration when holding the up arrow
        angleRad = newAngle * (pi / 180)
        acceleration = 70

        -- | acceleration in according direction
        (newVx, newVy)
            | SpecialKey KeyUp `elem` keys =
                (vx + acceleration * cos angleRad * dt,
                 vy + acceleration * sin angleRad * dt)
            | otherwise = (vx, vy)

        -- apply friction to slow player down 
        friction = 0.99
        finalVx = newVx * friction
        finalVy = newVy * friction

        -- Update position
        newX = x + finalVx * dt
        newY = y - finalVy * dt

        wrapX = wrapCoord (-400) 400 newX
        wrapY = wrapCoord (-300) 300 newY

    in p
        { playerPosition = (wrapX, wrapY)
        , playerVelocity = (finalVx, finalVy)
        , playerAngle = newAngle
        , shotCooldown = max 0 (shotCooldown p - dt)
        }

--Helpfunction to wrap coordinates around the screen edges
wrapCoord :: Float -> Float -> Float -> Float
wrapCoord min max val
    | val < min = max
    | val > max = min
    | otherwise = val

-- updates an asteroids position during time which follows the player
updateAsteroid :: Float -> Int -> Player -> Asteroid -> Asteroid
updateAsteroid dt currentScore p a =
    let
        (x, y) = asteroidPosition a
        (px, py) = playerPosition p

        dx = px - x
        dy = py - y

        distance = sqrt (dx * dx + dy * dy)

        -- Direction towards the player
        chase speed
            | distance > 0 =
                (dx / distance * speed, dy / distance * speed)
            | otherwise = asteroidVelocity a

        -- Different speeds for each enemy type
        normalSpeed = min 180 (35 + fromIntegral currentScore / 60)
        bigSpeed = min 105 (20 + fromIntegral currentScore / 150)
        fastSpeed = min 220 (100 + fromIntegral currentScore / 70)

        -- Determine the velocity based on the asteroid type
        (newVx, newVy) =
            case asteroidType a of
                Normal -> chase normalSpeed
                Big    -> chase bigSpeed
                Fast   ->
                    let (vx, vy) = asteroidVelocity a
                        oldSpeed = sqrt (vx * vx + vy * vy)
                    in if oldSpeed > 0
                       then (vx / oldSpeed * fastSpeed,
                             vy / oldSpeed * fastSpeed)
                       else chase fastSpeed

        newX = x + newVx * dt
        newY = y + newVy * dt

        wrapX = wrapCoord (-400) 400 newX
        wrapY = wrapCoord (-300) 300 newY

    in a
        { asteroidPosition = (wrapX, wrapY)
        , asteroidVelocity = (newVx, newVy)
        }

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
-- Handles user input
input :: Event -> GameState -> IO GameState

-- Restart the game when the user presses 'r'
input (EventKey (Char 'r') Down _ _) gstate = do
    gen <- newStdGen
    let startAstroids = makeRandomAsteroids 5 0 gen
    return ((initialState startAstroids)
        { highScores = highScores gstate })

-- Handle all other input
input e gstate =
    return (inputKey e (updateKeys e gstate))

-- Keeps track of which keys are currently pressed
updateKeys :: Event -> GameState -> GameState

updateKeys (EventKey key Down _ _) gstate =
    if key `elem` keysPressed gstate
        then gstate
        else gstate
            { keysPressed = key : keysPressed gstate }

updateKeys (EventKey key Up _ _) gstate =
    gstate
        { keysPressed = filter (/= key) (keysPressed gstate) }

updateKeys _ gstate = gstate

-- Handles individual key actions
inputKey :: Event -> GameState -> GameState

-- Pause the game when the user presses 'p'
inputKey (EventKey (Char 'p') Down _ _) gstate
    | gameOver gstate = gstate
    | otherwise = gstate { paused = not (paused gstate) }

-- Spacebar fires a bullet
inputKey (EventKey (SpecialKey KeySpace) Down _ _) gstate
    | shotCooldown (player gstate) > 0 = gstate
    | otherwise =
        let p = player gstate
            angleRad = playerAngle p * (pi / 180)
            bulletSpeed = 300
            bulletVx = bulletSpeed * cos angleRad
            bulletVy = - (bulletSpeed * sin angleRad)

            newBullet = Bullet
                { bulletPosition = playerPosition p
                , bulletVelocity = (bulletVx, bulletVy)
                , bulletTime = 0
                , bulletSize = 3
                }

        in gstate
            { bullets = newBullet : bullets gstate
            , player = p { shotCooldown = 0.7 }
            }

inputKey _ gstate = gstate

-- Calculates if there is a collision
collision :: Point -> Point -> Float -> Bool
collision (x1, y1) (x2, y2) size =
    let dx = x1 - x2
        dy = y1 - y2

    in sqrt (dx * dx + dy * dy) < size

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
    let
        -- Check hoeveel bullets deze asteroid raken
        numberOfHits a =
            length (filter (\b ->
                collision
                    (bulletPosition b)
                    (asteroidPosition a)
                    (asteroidSize a + bulletSize b)
                ) bullets)

        -- Haal voor iedere hit één health point weg
        damagedAsteroids =
            map (\a ->
                a { asteroidHealth =
                    asteroidHealth a - numberOfHits a
                  }) asteroids

        -- Asteroids met health boven nul blijven bestaan
        remainingAsteroids =
            filter (\a -> asteroidHealth a > 0) damagedAsteroids

        -- Asteroids met nul of minder health zijn vernietigd
        destroyedAsteroids =
            filter (\a -> asteroidHealth a <= 0) damagedAsteroids

        -- Bullets die een asteroid raken verdwijnen
        bulletHit b =
            any (\a ->
                collision
                    (bulletPosition b)
                    (asteroidPosition a)
                    (asteroidSize a + bulletSize b)
                ) asteroids

        remainingBullets =
            filter (not . bulletHit) bullets

        -- Alleen bij vernietiging ontstaat een explosie
        newExplosions =
            map (\a -> Explosion
                { explosionPosition = asteroidPosition a
                , explosionTime = 0.0
                }) destroyedAsteroids

        scoreGain = length destroyedAsteroids * 100

    in
        (remainingAsteroids, remainingBullets, newExplosions, scoreGain)

--random asteroid generater, spawns asteroids outside the screen and moves them inwards
makeRandomAsteroid :: Int -> StdGen -> (Asteroid, StdGen)
makeRandomAsteroid currentScore gen =
    let
        -- Kies willekeurig welk type asteroid verschijnt
        (typeRoll, gen1) = randomR (0 :: Int, 99) gen

        chosenType
            | currentScore >= 0 && typeRoll < 20 = Big --hier
            | currentScore >= 0 && typeRoll < 55 = Fast
            | otherwise = Normal

        -- Kies een willekeurige schermrand
        (edge, gen2) = randomR (0 :: Int, 3 :: Int) gen1

        (pos, gen3) = case edge of
            0 -> let (x, g) = randomR (-400, 400) gen2
                 in ((x, -300), g)
            1 -> let (x, g) = randomR (-400, 400) gen2
                 in ((x, 300), g)
            2 -> let (y, g) = randomR (-300, 300) gen2
                 in ((-400, y), g)
            _ -> let (y, g) = randomR (-300, 300) gen2
                 in ((400, y), g)

        -- Willekeurige bewegingsrichting
        (vx, gen4) = randomR (-50, 50) gen3
        (vy, gen5) = randomR (-50, 50) gen4

        -- Grootte afhankelijk van het type
        (size, gen6) = case chosenType of
            Normal -> randomR (20, 50) gen5
            Fast   -> randomR (10, 25) gen5
            Big    -> randomR (45, 65) gen5

        -- Health afhankelijk van het type
        health = case chosenType of
            Normal -> 1
            Fast   -> 1
            Big    -> 3

        asteroid = Asteroid
            { asteroidPosition = pos
            , asteroidVelocity = (vx, vy)
            , asteroidSize = size
            , asteroidType = chosenType
            , asteroidHealth = health
            }

    in (asteroid, gen6)


makeRandomAsteroids :: Int -> Int -> StdGen -> [Asteroid]
makeRandomAsteroids 0 _ _ = []
makeRandomAsteroids n currentScore gen =
    let
        (asteroid, gen') = makeRandomAsteroid currentScore gen
    in
        asteroid : makeRandomAsteroids (n - 1) currentScore gen'