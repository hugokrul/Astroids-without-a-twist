-- | This module contains the data types
--   which represent the state of the game
module Model where

data GameState = GameState {
                    world :: World,
                    elapsedTime :: Float
                  }

data World = Play  Player [Bullet]
           | Pause Player
           | GameOver
                    deriving (Show)

data Player = Player PointInSpace Velocity Acceleration Direction
  deriving (Show)

data Bullet = Bullet PointInSpace Velocity Direction LifeSpan
  deriving (Show)

-- Number
type Velocity     = Float

-- Degrees
type Direction    = Float

type LifeSpan     = Float

type Acceleration = (Float, Float)

-- (x, y)
type PointInSpace = (Float, Float)

initialStatePlayer :: Player
initialStatePlayer = Player (0, 0) 10 (0, 0) 0

initialState :: GameState
initialState = GameState { world = Play initialStatePlayer [], elapsedTime = 0}