-- | This module contains the data types
--   which represent the state of the game
module Model where

import Imports

import qualified Data.Set as Set

data GameState = GameState {
                    player :: Player,
                    astroids :: [Astroid],
                    bullets :: [Bullet],
                    elapsedTime :: Float,
                    playPauseGameOver :: State,
                    keySet :: Set.Set SpecialKey
                  }
                  deriving (Show)

data State = Play | Pause | GameOver
  deriving (Show, Eq)

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

data Size = Small | Medium | Big
  deriving (Show, Eq)

initialStatePlayer :: Player
initialStatePlayer = Player {positionPlayer=(0, 0), velocityPlayer=(0, 1), accelarationPlayer=(0, 0), lives=3}

testAstroid :: Astroid
testAstroid = Astroid {positionAstroid = (-20, 130), velocityAstroid = (10, 0), sizeAstroid = Big}

testBullet :: Bullet
testBullet = Bullet {positionBullet = (0, 0), velocityBullet = (0, 10)}

initialState :: GameState
initialState = GameState { player=initialStatePlayer, bullets=[], astroids=[], elapsedTime = 0, playPauseGameOver=Play, keySet = Set.empty}