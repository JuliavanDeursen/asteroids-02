-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step dt g
  | paused g   = return g
  | gameOver g = return g
  | otherwise  = return (updateGame dt g)

updateGame :: Float -> GameState -> GameState
updateGame dt g = 
let p'  = updatePlayer dt (player g)
      as' = map (updateAsteroid dt) (asteroids g)
      bs' = updateBullets dt (bullets g)
      ex' = updateExplosions dt (explosions g)

      -- collisions
      playerDead = playerHit p' as'
      (as2, ex2, scoreGain) = bulletHits bs' as'
  in g { player     = p'
       , asteroids  = as2
       , bullets    = bs'
       , explosions = ex' ++ ex2
       , score      = score g + scoreGain
       , gameOver   = playerDead
       }

-- updatePlayer
-- updateAsteroid
-- updateBullets
-- updateExplosions

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) gstate
  = -- If the user presses a character key, show that one
    gstate { infoToShow = ShowAChar c }
inputKey _ gstate = gstate -- Otherwise keep the same


collision :: Point -> Point -> Float -> Bool 
collision (x1,y1) (x2,y2) size = 
  let dx = x1 -x2
      dy = y1-y2
  in sqrt(dx*dx +dy*dy) <size --size is botsing radius, dus bijv asteroid en player

playerHit :: Player -> [Asteroid] -> Bool
--playerHit player asteroid_s = 