-- | this module contains the Main function which starts the game 
module Main where

import Controller
import Model
import View
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap
import System.Random
import HighScores


-- | starts the game by initializing a random generator, loading the high scores and creating the initial game state
main :: IO ()
main = do
    gen <- newStdGen
    scores <- loadHighScores

    let startAstroids = makeRandomAsteroids 5 0 gen

    let startState = (initialState startAstroids)
            { highScores = scores }

    playIO (InWindow "Asteroids" (800, 600) (0, 0))
              black
              60
              startState
              view
              input
              step