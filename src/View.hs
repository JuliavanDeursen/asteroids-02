-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view state
    | paused state   = return (pictures [renderGame state, renderPaused])
    | gameOver state = return (renderGameOver (score state))
    | otherwise      = return (renderGame state)

renderGame :: GameState -> Picture
renderGame state = pictures 
    [ renderPlayer (player state), 
      pictures (map renderAsteroid (asteroids state)), 
      pictures (map renderBullet (bullets state)), 
      pictures (map renderExplosion (explosions state)), 
      renderScore (score state)
    ]

renderPlayer :: Player -> Picture
renderPlayer p = 
    uncurry translate (playerPosition p) $ 
    rotate (playerAngle p) $ 
    color white $ 
    polygon [(-10, -10), (15, 0), (-10, 10), (5, 0)]

renderAsteroid :: Asteroid -> Picture
renderAsteroid a = 
    uncurry translate (asteroidPosition a) $
    color green $
    circleSolid (asteroidSize a)

renderBullet :: Bullet -> Picture
renderBullet b = 
    uncurry translate (bulletPosition b) $
    color red $ 
    circleSolid (bulletSize b)

--down here are functions that dont do anything yet
--so can be changed easily without breaking the game
renderExplosion :: Explosion -> Picture
renderExplosion e = 
    uncurry translate (explosionPosition e) $
    color orange $ 
    circle (explosionTime e * 10)

renderScore :: Int -> Picture
renderScore s = 
    translate (-320) 200 $ 
    scale 0.2 0.2 $ 
    color white $ 
    text ("Score: " ++ show s)

renderPaused :: Picture
renderPaused = pictures
    [ translate (-100) 0 $ scale 0.3 0.3 $ color yellow $ text "PAUSED"
    , translate (-150) (-50) $ scale 0.2 0.2 $ color white $ text "Press 'p' to resume"
    ]

renderGameOver :: Int -> Picture
renderGameOver finalScore = pictures
    [ translate (-150) 50 $ scale 0.4 0.4 $ color red $ text "GAME OVER"
    , translate (-180) (-20) $ scale 0.2 0.2 $ color white $ text ("Final Score: " ++ show finalScore)
    , translate (-220) (-70) $ scale 0.15 0.15 $ color white $ text "Press 'R' to restart"
    ]


--view = return . viewPure

--viewPure :: GameState -> Picture
--viewPure gstate = case infoToShow gstate of
--  ShowNothing   -> blank
--  ShowANumber n -> color green (text (show n))
--  ShowAChar   c -> color green (text [c])