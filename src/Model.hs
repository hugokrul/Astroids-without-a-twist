-- | This module contains the data types
--   which represent the state of the game
module Model where

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 1

data GameState = GameState {
                    world :: World,
                    elapsedTime :: Float
                  }

data World = Play  Player
           | Pause Player
           | GameOver
                    deriving (Show)

data Player = Player PointInSpace Velocity Acceleration Direction
  deriving (Show)

type Velocity     = Float
type Direction    = Float
type Acceleration = (Float, Float)
type PointInSpace = (Float, Float)

initialStatePlayer :: Player
initialStatePlayer = Player (0, 0) 10 (0, 0) 0

initialState :: GameState
initialState = GameState { world = Play initialStatePlayer, elapsedTime = 0}