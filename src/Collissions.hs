module Collissions where

import Imports
import Model
import Hits

checkCollissionPlanet :: Player -> [Planet] -> GameState -> GameState
checkCollissionPlanet p [] gstate= gstate
checkCollissionPlanet p [planet] gstate 
  | checkCollissionShipPlanet p planet = gstate { player = initialStatePlayer { lives = lives p - 1, reviving = True}}
  | otherwise = gstate
checkCollissionPlanet p (planet:rest) gstate
  | checkCollissionShipPlanet p planet = gstate { player = initialStatePlayer { lives = lives p - 1, reviving = True}}
  | otherwise = checkCollissionPlanet p rest gstate

checkCollissionAstroid :: Player -> [Astroid] -> GameState -> GameState
checkCollissionAstroid p [] gstate = gstate
checkCollissionAstroid p [a] gstate
  | checkCollissionShipAstroid p a = gstate { player = initialStatePlayer { lives = lives p - 1, reviving = True}}
  | otherwise = gstate
checkCollissionAstroid p (a : as) gstate
  | checkCollissionShipAstroid p a = gstate { player = initialStatePlayer { lives = lives p - 1, reviving = True}}
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