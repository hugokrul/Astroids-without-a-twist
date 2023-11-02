module Imports (
    module Graphics.Gloss,
    module Graphics.Gloss.Interface.IO.Game,
    module Graphics.Gloss.Data.Vector,
    module Graphics.Gloss.Geometry.Angle,
    module Graphics.Gloss.Data.Point,
    module Data.Maybe,
    module Data.List,
    module System.Random
    ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Data.Point
import Data.Maybe
import Data.List
import System.Random
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath

mapPlus :: (a -> Point) -> (a -> Point) -> a -> Point
mapPlus f1 f2 s = f1 s PMath.+ f2 s