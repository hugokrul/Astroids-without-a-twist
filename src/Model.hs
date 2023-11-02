-- | This module contains the data types
--   which represent the state of the game
module Model where

import qualified Data.Set as Set
-- be careful!

import Imports

data GameState = GameState
  { player :: Player,
    astroids :: [Astroid],
    bullets :: [Bullet],
    planets :: [Planet],
    elapsedTime :: Float,
    playPauseGameOver :: State,
    keySet :: Set.Set SpecialKey
  }
  deriving (Show)

data State = Play | Pause | GameOver | Start
  deriving (Show, Eq)

data Player = Player
  { positionPlayer :: PointInSpace,
    velocityPlayer :: Velocity,
    accelarationPlayer :: Acceleration,
    lives :: Float
  }
  deriving (Show)

data Bullet = Bullet
  { positionBullet :: PointInSpace,
    velocityBullet :: Velocity
  }
  deriving (Show, Eq)

data Astroid = Astroid
  { positionAstroid :: PointInSpace,
    velocityAstroid :: Velocity,
    sizeAstroid :: Size
  }
  deriving (Show, Eq)

data Planet = Planet
  { positionPlanet :: PointInSpace,
    velocityPlanet :: PointInSpace,
    lifeSpanPlanet :: Float
  }
  deriving (Show, Eq)

type Velocity = Vector

type LifeSpan = Float

type Acceleration = Vector

type PointInSpace = Point

data Size = Small | Medium | Big
  deriving (Show, Eq)

initialStatePlayer :: Player
initialStatePlayer = Player {positionPlayer = (0, 0), velocityPlayer = (0, 0.15), accelarationPlayer = (0, 0), lives = 3}

testPlanet :: Planet
testPlanet = Planet {positionPlanet = (-300, -300), velocityPlanet = (50, 100), lifeSpanPlanet = 0}

testAstroid :: Astroid
testAstroid = Astroid {positionAstroid = (100, 0), velocityAstroid = (10, 0), sizeAstroid = Big}

testBullet :: Bullet
testBullet = Bullet {positionBullet = (0, 0), velocityBullet = (0, 10)}

initialState :: GameState
initialState = GameState {player = initialStatePlayer, bullets = [], astroids = [testAstroid], planets = [testPlanet], elapsedTime = 0, playPauseGameOver = Start, keySet = Set.empty}