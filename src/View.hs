-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

-- | displays the screen, depending on the current gamestate
view :: GameState -> IO Picture
view state
    | gameOver state = return (renderGameOver (score state) (highScores state))
    | paused state   = return (pictures [renderGame state, renderPaused])
    | otherwise      = return (renderGame state)

-- | combines all aspects into one picture 
renderGame :: GameState -> Picture
renderGame state = pictures 
    [ renderPlayer (player state), 
      pictures (map renderAsteroid (asteroids state)), 
      pictures (map renderBullet (bullets state)), 
      pictures (map renderExplosion (explosions state)), 
      renderScore (score state)
    ]

-- | draws the player as a white spaceship at current position
renderPlayer :: Player -> Picture
renderPlayer p = 
    uncurry translate (playerPosition p) $ 
    rotate (playerAngle p) $ 
    color white $ 
    polygon [(-10, -10), (15, 0), (-10, 10), (5, 0)]

-- | draws an asteroid at the current position, color and size depending on the type 
renderAsteroid :: Asteroid -> Picture
renderAsteroid a =
    let
        asteroidColor = case asteroidType a of -- | different asteroids have different colors
            Normal -> green
            Fast   -> red
            Big    -> magenta 

        s = asteroidSize a / 25 -- | scale according to size

    in
        uncurry translate (asteroidPosition a) $
        color asteroidColor $
        scale s s $
        polygon
            [ (-18, 22)
            , (12, 28)
            , (26, 10)
            , (22, -8)
            , (5, -28)
            , (-15, -24)
            , (-28, -5)
            , (-22, 14)
            ]
-- | draws the bullet as a small red circle at current position
renderBullet :: Bullet -> Picture
renderBullet b = 
    uncurry translate (bulletPosition b) $
    color red $ 
    circleSolid (bulletSize b)

-- | draws an animated explosion using multiple particles, these spread and fade away
renderExplosion :: Explosion -> Picture
renderExplosion e =
    let t = explosionTime e
        duration = 1.2 
        progress = min 1 (t / duration)
        (x, y) = explosionPosition e

        -- | calculates size and colour of particles 
        particle i =
            let angle = fromIntegral i * (2 * pi / 12) -- create 12 particles 

                distance = progress * (30 + fromIntegral (i `mod` 4) * 12)

                px = distance * cos angle
                py = distance * sin angle

                size = (1 - progress) * (4 + fromIntegral (i `mod` 3) * 2)
                -- | further away = lighter color
                particleColor = case i `mod` 3 of
                    0 -> yellow
                    1 -> orange
                    _ -> red

            in translate px py $
               color (withAlpha (1 - progress) particleColor) $
               circleSolid size

        -- creates a flash in the middle which gradually gets smaller and transparant 
        flash =
            color (withAlpha (1 - progress) yellow) $
            circleSolid (20 * (1 - progress))

    in translate x y $
       pictures (flash : map particle [0..11])

-- | displays the current score, top left corner
renderScore :: Int -> Picture
renderScore s = 
    translate (-320) 200 $ 
    scale 0.2 0.2 $ 
    color white $ 
    text ("Score: " ++ show s)

-- | displays pause screen 
renderPaused :: Picture
renderPaused = pictures
    [ translate (-100) 0 $ scale 0.3 0.3 $ color yellow $ text "PAUSED"
    , translate (-150) (-50) $ scale 0.2 0.2 $ color white $ text "Press 'P' to resume"
    ]

-- | displays game over screen 
renderGameOver :: Int -> [Int] -> Picture
renderGameOver finalScore scores = pictures
    [ translate (-150) 200 $
        scale 0.4 0.4 $
        color red $
        text "GAME OVER"

    , translate (-150) 140 $
        scale 0.2 0.2 $
        color white $
        text ("Final Score: " ++ show finalScore) -- | with the final score 

    , translate (-150) 80 $
        scale 0.2 0.2 $
        color yellow $
        text "HIGH SCORES" -- | and the current top 10 high scores 

    , pictures
        [ translate (-120) (40 - fromIntegral i * 22) $
            scale 0.18 0.18 $
            color white $
            text (show i ++ ". " ++ show s)
        | (i, s) <- zip [1 :: Int ..] scores
        ]

    , translate (-150) (-240) $
        scale 0.15 0.15 $
        color white $
        text "Press 'R' to restart" -- | by pressing R the game restarts
    ]