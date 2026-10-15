-- | This module contains the data types
--   which represent the state of the game
module Model where

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState 
{ player :: Player, 
asteroids :: [Asteroid], 
bullets :: [Bullet], 
explosions :: [Explosion], 
score :: Int, 
paused :: Bool,  
gameOver :: Bool 
} 

data Player = Player 
{ playerPosition :: Point, 
playerVelocity :: Velocity, 
playerAngle :: Float, 
playerSize :: Float 
} 
data Asteroid = Asteroid 
{ asteroidPosition :: Point, 
asteroidVelocity :: Velocity, 
asteroidSize :: Float 
} 
data Bullet = Bullet 
{ bulletPosition :: Point, 
bulletVelocity :: Velocity, 
bulletTime :: Float  
} 
data Explosion = Explosion 
{ explosionPosition :: Point, 
explosionTime     :: Float  
} 
type Point = (Float, Float) 
type Velocity = (Float, Float)

collision :: Point -> Point -> Float -> Bool 

playerHit :: Player -> [Asteroid] -> Bool

initialState :: GameState
initialState = GameState ShowNothing 0