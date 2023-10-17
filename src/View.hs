-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Ship

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
--viewPure gstate = (translate (fst (givePIS (world gstate))) (snd (givePIS(world gstate)))) rotate (giveDir (world gstate)) ship
viewPure gstate = pictures [
        uncurry translate (givePIS (world gstate)) (rotate (giveDir (world gstate)) ship)
    ]

givePIS :: World -> PointInSpace
givePIS (Play (Player p _ _ _)) = p

giveVelocity :: World -> Velocity
giveVelocity (Play (Player _ v _ _)) = v

giveAcceleration :: World -> Acceleration
giveAcceleration (Play (Player _ _ a _)) = a

giveDir :: World -> Direction
giveDir (Play (Player _ _ _ d)) = d

-- case infoToShow gstate of
--   ShowStart     -> ship
--   MoveUp x      -> translate 0 x ship
--   ShowANumber n -> color green (text (show n))
--   ShowAChar   c -> color green (text [c])