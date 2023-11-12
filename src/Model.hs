-- | This module contains the data types
--   which represent the state of the game
module Model where

import qualified Data.Set as Set
import Imports

data GameState = GameState
  { player :: Player,
    astroids :: [Astroid],
    bullets :: [Bullet],
    planets :: [Planet],
    enemy :: [Enemy],
    elapsedTime :: Float,
    playPauseGameOver :: State,
    keySet :: Set.Set SpecialKey,
    highScore :: Float,
    score :: Float
  }
  deriving (Show)


data Enemy = Enemy
  { positionEnemy :: PointInSpace,
    velocityEnemy :: Velocity,
    lifeSpanEnemy :: Float,
    reloading :: Bool
  }
  deriving (Show)

data Player = Player
  { positionPlayer :: PointInSpace,
    velocityPlayer :: Velocity,
    accelarationPlayer :: Acceleration,
    lives :: Float,
    reviving :: Bool,
    deathAnimationTime :: Float,
    deathPosition :: PointInSpace,
    deathVelocity :: Velocity
  }
  deriving (Show)

data Bullet = Bullet
  { positionBullet :: PointInSpace,
    velocityBullet :: Velocity,
    enemyBullet :: Bool
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

data Size         = Small | Medium | Big
  deriving (Show, Eq)

data State        = Play  | Pause  | GameOver | Start
  deriving (Show, Eq)

type Velocity     = Vector

type LifeSpan     = Float

type Acceleration = Vector

type PointInSpace = Point


initialPlayer :: Player
initialPlayer = Player {positionPlayer = (0, 0), velocityPlayer = (0, 0.15), accelarationPlayer = (0, 0), lives = 3, reviving = False, deathAnimationTime = 0.0, deathPosition = (2000, 2000), deathVelocity = (0, 0)}

initialEnemy       :: Enemy
initialEnemy       = Enemy {positionEnemy = (100, 100), velocityEnemy = (0, 0), lifeSpanEnemy = 0, reloading = False}

initialState       :: GameState
initialState       = GameState {player = initialPlayer, bullets = [], astroids = [], planets = [], enemy = [], elapsedTime = 0, playPauseGameOver = Start, keySet = Set.empty, highScore = 0.0, score = 0.0}
