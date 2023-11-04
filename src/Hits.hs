module Hits where

import Imports
import Model
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath

pointInAstroid :: Point -> Astroid -> Bool
pointInAstroid p0 a = case sizeAstroid a of
  Big -> pointInBox p0 p1 p2
    where
      p1 = (43, 40) PMath.+ pos
      p2 = (-43.4, -38.3) PMath.+ pos
      pos = positionAstroid a
  Medium -> pointInBox p0 p1 p2
    where
      p1 = (21.5, 20) PMath.+ pos
      p2 = (-17.2, -19.15) PMath.+ pos
      pos = positionAstroid a
  Small -> pointInBox p0 p1 p2
    where
      p1 = (8.6, 8) PMath.+ pos
      p2 = (-6.88, -7.66) PMath.+ pos
      pos = positionAstroid a

pointInPlanet :: Point -> Planet -> Bool
pointInPlanet p0 planet = pointInBox p0 p1 p2
  where
    p1 = (planetX+75, planetY-75)
    p2 = (planetX-75, planetY+75)
    (planetX, planetY) = positionPlanet planet