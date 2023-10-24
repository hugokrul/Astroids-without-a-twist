module Imports (
    module Graphics.Gloss,
    module Graphics.Gloss.Interface.IO.Game
    ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath

mapPlus :: (a -> Point) -> (a -> Point) -> a -> Point