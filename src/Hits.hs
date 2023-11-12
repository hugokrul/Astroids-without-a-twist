module Hits where

import Imports
import Model
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath

-- Checks if a point is in the hitbox, for every astroid.
pointInAstroid :: Point -> Astroid -> Bool
pointInAstroid p0 a = case sizeAstroid a of
  Big     -> pointInBox p0 p1 p2
    where
      p1  = (43, 40) PMath.+ pos
      p2  = (-43.4, -38.3) PMath.+ pos
      pos = positionAstroid a
  Medium  -> pointInBox p0 p1 p2
    where
      p1  = (21.5, 20) PMath.+ pos
      p2  = (-17.2, -19.15) PMath.+ pos
      pos = positionAstroid a
  Small   -> pointInBox p0 p1 p2
    where
      p1  = (8.6, 8) PMath.+ pos
      p2  = (-6.88, -7.66) PMath.+ pos
      pos = positionAstroid a

-- Checks if a point is in a planet.
pointInPlanet :: Point -> Planet -> Bool
pointInPlanet p0 planet = pointInBox p0 p1 p2
  where
    p1 = (planetX+75, planetY-75)
    p2 = (planetX-75, planetY+75)
    (planetX, planetY) = positionPlanet planet

-- Checks if a point is in an enemy.
pointInEnemy :: Point -> Enemy -> Bool
pointInEnemy p0 enemy =
  pointInBox p0 p1 p2 || pointInBox p0 p3 p4
  where
    (x, y) = positionEnemy enemy
    p1 = (x + 40, y + 15)
    p2 = (x - 40, y - 15)
    p3 = (x + 15, y + 35)
    p4 = (x - 15, y + 10)

-- Checks if a point is in a ship.
pointInShip :: Point -> Player -> Bool
pointInShip p0 player = pointInBox p0 p1 p2
  where
    p1 = (x + 17.5, y + 17.5)
    p2 = (x - 17.5, y - 17.5)
    pos@(x, y) = positionPlayer player