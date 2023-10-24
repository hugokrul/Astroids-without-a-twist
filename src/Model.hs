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
                           lives :: Float
                          }
                            deriving (Show)

data Bullet   = Bullet    {
                            positionBullet :: PointInSpace,
                            velocityBullet :: Velocity
                          }
                            deriving (Show, Eq)

data Astroid  = Astroid   {
                            positionAstroid :: PointInSpace,
                            velocityAstroid :: Velocity,
                            sizeAstroid :: Size
                          }
                            deriving (Show, Eq)

type Velocity     = Vector

type LifeSpan     = Float

type Acceleration = Vector

type PointInSpace = Point

data Size = Big | Medium | Small
  deriving (Show, Eq)

initialStatePlayer :: Player
initialStatePlayer = Player {positionPlayer=(0, 0), velocityPlayer=(0, 10), accelarationPlayer=(0, 0), lives=3}

initialAstroid :: Astroid
initialAstroid = Astroid {positionAstroid = (-20, 130), velocityAstroid = (0, 0), sizeAstroid = Big}

initialBullet :: Bullet
initialBullet = Bullet {positionBullet = (0, 0), velocityBullet = (0, 10)}

initialState :: GameState
initialState = GameState { player=initialStatePlayer, bullets=[], astroids=[initialAstroid], elapsedTime = 0, playPauseGameOver=Play}