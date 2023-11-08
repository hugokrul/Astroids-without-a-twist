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
    enemy :: [Enemy],
    elapsedTime :: Float,
    playPauseGameOver :: State,
    keySet :: Set.Set SpecialKey,
    highScore :: Float,
    score :: Float
  }
  deriving (Show)

data State = Play | Pause | GameOver | Start
  deriving (Show, Eq)

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
    reviving :: Bool
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

type Velocity = Vector

type LifeSpan = Float

type Acceleration = Vector

type PointInSpace = Point

data Size = Small | Medium | Big
  deriving (Show, Eq)

initialStatePlayer :: Player
initialStatePlayer = Player {positionPlayer = (0, 0), velocityPlayer = (0, 0.15), accelarationPlayer = (0, 0), lives = 3, reviving = False}

testPlanet :: Planet
testPlanet = Planet {positionPlanet = (-300, -350), velocityPlanet = (50, 100), lifeSpanPlanet = 0}

testAstroid :: Astroid
testAstroid = Astroid {positionAstroid = (-20, 130), velocityAstroid = (100, 0), sizeAstroid = Big}

testBullet :: Bullet
testBullet = Bullet {positionBullet = (0, 0), velocityBullet = (0, 10), enemyBullet = False}

initialEnemy :: Enemy
initialEnemy = Enemy {positionEnemy = (100, 100), velocityEnemy = (0, 0), lifeSpanEnemy = 0, reloading = False}

initialState :: GameState
initialState = GameState {player = initialStatePlayer, bullets = [], astroids = [], planets = [], enemy = [], elapsedTime = 0, playPauseGameOver = Pause, keySet = Set.empty, highScore = 0.0, score = 0.0}
