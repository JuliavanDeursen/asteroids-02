-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss

type Velocity = (Float, Float)

--gamestate data type, to hold information about the game during the game    
data GameState = GameState
    { player :: Player
    , asteroids :: [Asteroid]
    , bullets :: [Bullet]
    , explosions :: [Explosion]
    , score :: Int
    , paused :: Bool
    , gameOver :: Bool
    }

--player data type 
data Player = Player 
    { playerPosition :: Point, 
      playerVelocity :: Velocity, 
      playerAngle :: Float, 
      playerSize :: Float 
    } 

--asteroid data type
data Asteroid = Asteroid 
    { asteroidPosition :: Point, 
      asteroidVelocity :: Velocity, 
      asteroidSize :: Float 
    } 

--bullet data type
data Bullet = Bullet 
    { bulletPosition :: Point, 
      bulletVelocity :: Velocity, 
      bulletTime :: Float, 
      bulletSize :: Float  
    } 

--explosion data type
data Explosion = Explosion 
    { explosionPosition :: Point, 
      explosionTime     :: Float  -- to keep track for the animation time
    } 

initialState :: GameState
initialState = GameState 
    { player = Player {playerPosition = (0,0), playerVelocity = (0,0), playerAngle = 0, playerSize = 20}, 
      asteroids = [Asteroid {asteroidPosition = (100, 100), asteroidVelocity = (50, -30), asteroidSize = 30},
                   Asteroid {asteroidPosition = (-150, 50), asteroidVelocity = (-30, -5), asteroidSize = 40},
                   Asteroid {asteroidPosition = (200, -100), asteroidVelocity = (20, 50), asteroidSize = 25}], 
      bullets = [],   
      explosions = [], 
      score = 0, 
      paused = False,  
      gameOver = False 
    }
--comments = 
--the spawning of asteroids needs to be outside frame, so that they spawn outside the screen and move inwards
--for now they are hardcoded but that needs to be changed to random spawning, size and velocity


--playingState :: GameState
--playingState = 