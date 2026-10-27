-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view state
    | paused state   = pictures [renderGame state, renderPaused]
    | gameOver state = renderGameOver (score state)
    | otherwise      = renderGame state



--view = return . viewPure

--viewPure :: GameState -> Picture
--viewPure gstate = case infoToShow gstate of
--  ShowNothing   -> blank
--  ShowANumber n -> color green (text (show n))
--  ShowAChar   c -> color green (text [c])