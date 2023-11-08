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

checkCollissionShipEnemy :: Player -> [Enemy] -> GameState -> GameState
checkCollissionShipEnemy _ [] gstate = gstate
checkCollissionShipEnemy p (e:_) gstate = if 
  pointInShip e (x + 17.5, y - 25)
    || pointInShip e (x - 17.5, y - 25)
    || pointInShip e (x + 17.5, y - 25)
    || pointInShip e (x + 17.5, y + 25)
    then gstate { player = initialStatePlayer { lives = lives p - 1, reviving = True}}
    else gstate
  where
    (x, y) = positionPlayer p

checkCollissionPointShip :: Player -> Point -> Bool
checkCollissionPointShip player p0 = pointInBox p0 p1 p2
  where
    p1 = (x + 17.5, y + 17.5)
    p2 = (x - 17.5, y - 17.5)
    pos@(x, y) = positionPlayer player

pointInShip :: Enemy -> Point -> Bool
pointInShip enemy p0 = 
  pointInBox p0 p1 p2 || pointInBox p0 p3 p4
  where
    (x, y) = positionEnemy enemy
    p1 = (x+40, y+15)
    p2 = (x-40, y-15)
    p3 = (x+15, y+35)
    p4 = (x-15, y+10)