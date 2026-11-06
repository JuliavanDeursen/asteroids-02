-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss
import System.Random
import Graphics.Gloss.Interface.IO.Game

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
    , keysPressed :: [Key]
    , highScores :: [Int]
    }

--player data type 
data Player = Player 
    { playerPosition :: Point, 
      playerVelocity :: Velocity, 
      playerAngle :: Float, 
      playerSize :: Float,
      shotCooldown :: Float --makes sure the player can't shoot too fast
    } 

--asteroid data type, different kind of enemies with different behaviour
data AsteroidType = Normal | Fast | Big
    deriving (Eq, Show)

data Asteroid = Asteroid
    { asteroidPosition :: Point
    , asteroidVelocity :: Velocity
    , asteroidSize :: Float
    , asteroidType :: AsteroidType
    , asteroidHealth :: Int -- | for the Big asteroids, needs 3 hits to eliminate 
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

-- | to start the game 
initialState :: [Asteroid] -> GameState
initialState startAsteroids = GameState 
    { player = Player {playerPosition = (0,0), playerVelocity = (0,0), playerAngle = 0, playerSize = 20, shotCooldown = 0}, 
      asteroids = startAsteroids, 
      bullets = [],   
      explosions = [], 
      score = 0, 
      paused = False,  
      gameOver = False,
      keysPressed = [],
      highScores = [] -- | every list empty, filled when starting/playing the game 
    }