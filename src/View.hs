{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use :" #-}
-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
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
                            getPictures gstate
                        ]
    Pause       -> pictures [color white $ translate (-400) 200 $ scale 0.5 0.25 $ text "pausing"]
    GameOver    -> Pictures [color white $ translate (-400) 200 $ scale 0.5 0.25 $ text "Game Over"]
    

getPictures :: GameState -> Picture
getPictures gstate = pictures (
    map showBullet (bullets gstate) ++ 
    [uncurry translate (positionPlayer (player gstate)) $ rotate (directionPlayer (player gstate)) ship]
    )

getTime :: GameState -> Float
getTime = elapsedTime