module Collissions where

import Hits
import Imports
import Model

-- checks if there is a collissions between the player and all the planets.
checkCollissionPlanet :: Player -> [Planet] -> GameState -> GameState
checkCollissionPlanet p []       gstate = gstate
checkCollissionPlanet p [planet] gstate
  | checkCollissionShipPlanet p planet = gstate {player = initialPlayer {lives = lives p - 1, reviving = True, deathAnimationTime = (elapsedTime gstate + 3.0), deathPosition = positionPlayer (p), deathVelocity = velocityPlayer p}}
  | otherwise                          = gstate
checkCollissionPlanet p (planet : rest) gstate
  | checkCollissionShipPlanet p planet = gstate {player = initialPlayer {lives = lives p - 1, reviving = True, deathAnimationTime = (elapsedTime gstate + 3.0), deathPosition = positionPlayer (p), deathVelocity = velocityPlayer p}}
  | otherwise                          = checkCollissionPlanet p rest gstate

-- checks if there is a collission between then player and all the Astroids.
checkCollissionAstroid :: Player -> [Astroid] -> GameState -> GameState
checkCollissionAstroid p [] gstate = gstate
checkCollissionAstroid p [a] gstate
  | checkCollissionShipAstroid p a = gstate {player = initialPlayer {lives = lives p - 1, reviving = True, deathAnimationTime = (elapsedTime gstate + 3.0), deathPosition = positionPlayer (p), deathVelocity = velocityPlayer p}}
  | otherwise                      = gstate
checkCollissionAstroid p (a : as) gstate
  | checkCollissionShipAstroid p a = gstate {player = initialPlayer {lives = lives p - 1, reviving = True, deathAnimationTime = (elapsedTime gstate + 3.0), deathPosition = positionPlayer (p), deathVelocity = velocityPlayer p}}
  | otherwise                      = checkCollissionAstroid p as gstate

-- Checks if there is a collission between the player and one planet.
checkCollissionShipPlanet :: Player -> Planet -> Bool
checkCollissionShipPlanet p planet =
       pointInPlanet (x + 17.5, y - 25) planet
    || pointInPlanet (x - 17.5, y - 25) planet
    || pointInPlanet (x + 17.5, y - 25) planet
    || pointInPlanet (x + 17.5, y + 25) planet
  where
    (x, y) = positionPlayer p

-- Checks if there is a collission between the player and one Astroid.
checkCollissionShipAstroid :: Player -> Astroid -> Bool
checkCollissionShipAstroid p a =
       pointInAstroid (x + 17.5, y - 25) a
    || pointInAstroid (x - 17.5, y - 25) a
    || pointInAstroid (x + 17.5, y - 25) a
    || pointInAstroid (x + 17.5, y + 25) a
  where
    (x, y) = positionPlayer p

-- Checks if there is a collission between the player and the enemy, returns a game state because it doens't need recursion.
checkCollissionShipEnemy :: Player -> [Enemy] -> GameState -> GameState
checkCollissionShipEnemy _ [] gstate = gstate
checkCollissionShipEnemy p (e : _) gstate =
  if   pointInEnemy (x + 17.5, y - 25) e
    || pointInEnemy (x - 17.5, y - 25) e
    || pointInEnemy (x + 17.5, y - 25) e
    || pointInEnemy (x + 17.5, y + 25) e
    then gstate {player = initialPlayer {lives = lives p - 1, reviving = True}}
    else gstate
  where
    (x, y) = positionPlayer p