module Imports
  ( module Graphics.Gloss,
    module Graphics.Gloss.Interface.IO.Game,
    module Graphics.Gloss.Data.Vector,
    module Graphics.Gloss.Geometry.Angle,
    module Graphics.Gloss.Data.Point,
    module Data.Maybe,
    module Data.List,
    module System.Random,
  )
where

import Data.List
import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Data.Point
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Interface.IO.Game
import System.Random

mapPlus :: (a -> Point) -> (a -> Point) -> a -> Point
mapPlus f1 f2 s = f1 s PMath.+ f2 s
