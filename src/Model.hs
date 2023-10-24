-- | This module contains the data types
--   which represent the state of the game
module Model where

import System.Random
import Imports

data GameState = GameState {
                    player :: Player,
                    astroids :: [Astroid],
                    bullets :: [Bullet],
                    elapsedTime :: Float,
                    playPauseGameOver :: State
                  }

data State = Play | Pause | GameOver

data Player   = Player    {
                           positionPlayer :: PointInSpace,
                           velocityPlayer :: Velocity,
                           accelarationPlayer :: Acceleration,
                           directionPlayer :: Direction
                          }
                            deriving (Show)

data Bullet   = Bullet    {
                            positionBullet :: PointInSpace,
                            velocityBullet :: Velocity,
                            lifeSpanBullet :: LifeSpan
                          }
                            deriving (Show)

data Astroid  = Astroid   {
                            positionAstroid :: PointInSpace,
                            velocityAstroid :: Velocity,
                            directionAstroid :: Direction,
                            lifeSpanAstroid :: LifeSpan,
                            sizeAstroid :: Size
                          }
                            deriving (Show)

type Velocity     = Vector

type LifeSpan     = Float

type Acceleration = Vector

-- (x, y)
type PointInSpace = Point

data Size = Big | Medium | Small
  deriving (Show)

get = mkStdGen 2023

initialStatePlayer :: Player
initialStatePlayer = Player {positionPlayer=(0, 0), velocityPlayer=(10, 0), accelarationPlayer=(0, 0)}

initialState :: GameState
initialState = GameState { player=initialStatePlayer, bullets=[], astroids=[Astroid (0, 0) 10 100 0 Big], elapsedTime = 0, playPauseGameOver=Play}