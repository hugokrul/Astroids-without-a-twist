module Imports (
    module Graphics.Gloss,
    module Graphics.Gloss.Interface.IO.Game,
    module Graphics.Gloss.Data.Vector,
    module Graphics.Gloss.Geometry.Angle
    ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath

mapPlus :: (a -> Point) -> (a -> Point) -> a -> Point
mapPlus f1 f2 s = f1 s PMath.+ f2 s