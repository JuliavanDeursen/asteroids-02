-- | This module contains the data types
--   which represent the state of the game
module Model where

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

--gamestate data type, to hold information about the game during the game
data GameState = GameState 
{ player :: Player, 
asteroids :: [Asteroid], 
bullets :: [Bullet], 
explosions :: [Explosion], 
score :: Int, 
paused :: Bool,  
gameOver :: Bool 
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
bulletTime :: Float  
} 

--explosion data type
data Explosion = Explosion 
{ explosionPosition :: Point, 
explosionTime     :: Float  -- to keep track for the animation time
} 

type Point = (Float, Float) 
type Velocity = (Float, Float)

initialState :: GameState
initialState = GameState 
{
    player = Player (0,0) (0,0) 0 20, 
asteroids = [], 
bullets = [], 
explosions = [], 
score = 0, 
paused = False,  
gameOver = False 
}

playingState :: GameState
--playingState = 