module Collissions where

import Data.IntMap (partition)
import Hits
import Imports
import Model

checkCollissionPlanet :: Player -> [Planet] -> GameState -> GameState
checkCollissionPlanet p [] gstate = gstate
checkCollissionPlanet p [planet] gstate
  | checkCollissionShipPlanet p planet = gstate {player = initialStatePlayer {lives = lives p - 1, reviving = True, deathAnimationTime = (elapsedTime gstate + 3.0), deathPosition = positionPlayer (p), deathVelocity = velocityPlayer p}}
  | otherwise = gstate
checkCollissionPlanet p (planet : rest) gstate
  | checkCollissionShipPlanet p planet = gstate {player = initialStatePlayer {lives = lives p - 1, reviving = True, deathAnimationTime = (elapsedTime gstate + 3.0), deathPosition = positionPlayer (p), deathVelocity = velocityPlayer p}}
  | otherwise = checkCollissionPlanet p rest gstate

checkCollissionAstroid :: Player -> [Astroid] -> GameState -> GameState
checkCollissionAstroid p [] gstate = gstate
checkCollissionAstroid p [a] gstate
  | checkCollissionShipAstroid p a = gstate {player = initialStatePlayer {lives = lives p - 1, reviving = True, deathAnimationTime = (elapsedTime gstate + 3.0), deathPosition = positionPlayer (p), deathVelocity = velocityPlayer p}}
  | otherwise = gstate
checkCollissionAstroid p (a : as) gstate
  | checkCollissionShipAstroid p a = gstate {player = initialStatePlayer {lives = lives p - 1, reviving = True, deathAnimationTime = (elapsedTime gstate + 3.0), deathPosition = positionPlayer (p), deathVelocity = velocityPlayer p}}
  | otherwise = checkCollissionAstroid p as gstate

checkCollissionShipPlanet :: Player -> Planet -> Bool
checkCollissionShipPlanet p planet =
  pointInPlanet (x + 17.5, y - 25) planet
    || pointInPlanet (x - 17.5, y - 25) planet
    || pointInPlanet (x + 17.5, y - 25) planet
    || pointInPlanet (x + 17.5, y + 25) planet
  where
    (x, y) = positionPlayer p

checkCollissionShipAstroid :: Player -> Astroid -> Bool
checkCollissionShipAstroid p a =
  pointInAstroid (x + 17.5, y - 25) a
    || pointInAstroid (x - 17.5, y - 25) a
    || pointInAstroid (x + 17.5, y - 25) a
    || pointInAstroid (x + 17.5, y + 25) a
  where
    (x, y) = positionPlayer p

checkCollissionShipEnemy :: Player -> [Enemy] -> GameState -> GameState
checkCollissionShipEnemy _ [] gstate = gstate
checkCollissionShipEnemy p (e : _) gstate =
  if pointInShip e (x + 17.5, y - 25)
    || pointInShip e (x - 17.5, y - 25)
    || pointInShip e (x + 17.5, y - 25)
    || pointInShip e (x + 17.5, y + 25)
    then gstate {player = initialStatePlayer {lives = lives p - 1, reviving = True}}
    else gstate
  where
    (x, y) = positionPlayer p

checkCollissionPointShip :: Player -> Point -> Bool
checkCollissionPointShip player p0 = pointInBox p0 p1 p2
  where
    p1 = (x + 17.5, y + 17.5)
    p2 = (x - 17.5, y - 17.5)
    pos@(x, y) = positionPlayer player