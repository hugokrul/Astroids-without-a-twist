{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use :" #-}
-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Model
import Ship
import Bullet
import Astroid
import GHC.Float (showFloat)

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case playPauseGameOver gstate of 
    Play        -> pictures [
                                getPictures gstate,
                                color white $ translate (-400) 200 $ scale 0.5 0.25 $ text $ show $ lives $ player gstate
                            ]
    Pause       -> pictures [
                                getPictures gstate,
                                color white $ translate (-400) 200 $ scale 0.5 0.25 $ text $ show $ lives $ player gstate
                            ]
    GameOver    -> Pictures [color white $ translate (-400) 200 $ scale 0.5 0.25 $ text "Game Over"]
    

getPictures :: GameState -> Picture
getPictures gstate = pictures (
        map showBullet (bullets gstate) ++ 
        map showAstroid (astroids gstate) ++
        [uncurry translate (positionPlayer (player gstate)) $ rotate deg ship]
    )
    where
        (x,y) = velocityPlayer (player gstate)
        deg = radToDeg (argV(y,x))


getTime :: GameState -> Float
getTime = elapsedTime