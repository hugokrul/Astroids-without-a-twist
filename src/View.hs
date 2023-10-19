{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use :" #-}
-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Ship
import Bullet
import GHC.Float (showFloat)

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = pictures [
        getPictures (world gstate) gstate,
        color white $ translate (-400) 200 $ scale 0.5 0.25 $ text $ show $ getTime gstate
    ]

getPictures :: World -> GameState -> Picture
getPictures world@(Play (Player p v a d) bullets) gstate = pictures (
    [uncurry translate p $ rotate d ship] ++ 
    getBulletsPicture world (getTime gstate)
    )

getTime :: GameState -> Float
getTime = elapsedTime