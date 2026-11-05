module Main where

import Controller
import Model
import View
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap
import System.Random

main :: IO ()
main = do
    gen <- newStdGen
    let startAstroids = makeRandomAsteroids 5 gen
    
    playIO (InWindow "Asteroids" (800, 600) (0, 0)) -- Or FullScreen
              black            -- Background color
              60               -- Frames per second
              (initialState startAstroids)     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function