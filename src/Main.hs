module Main where

import Controller
import Model
import View
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap

main :: IO ()
main = playIO (InWindow "Asteroids" (800, 600) (0, 0)) -- Or FullScreen
              black            -- Background color
              60               -- Frames per second
              initialState     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function