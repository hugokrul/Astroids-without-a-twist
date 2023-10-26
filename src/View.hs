{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use :" #-}
-- | This module defines how to turn
--   the game state into a picture
module View where

import Imports
import Model
import Ship
import Bullet
import Astroid

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case playPauseGameOver gstate of
    Play        -> pictures [
                                getPictures gstate,
                                color white $ translate (-400) 200 $ scale 0.5 0.25 $ text $ "Lives:" ++ show (lives $ player gstate)
                            ]
    Pause       -> pictures [
                                getPictures gstate,
                                color white $ translate (-400) 200 $ scale 0.5 0.25 $ text $ "Lives:" ++ show (lives $ player gstate),
                                color white $ translate (-400) 150 $ scale 0.5 0.25 $ text "Paused"
                            ]
    GameOver    -> Pictures [color white $ translate (-400) 200 $ scale 0.5 0.25 $ text "Game Over",
                             color white $ translate (-400) 150 $ scale 0.5 0.25 $ text "Press Esc to play again"]


getPictures :: GameState -> Picture
getPictures gstate = pictures (
        map showBullet (bullets gstate) ++
        map showAstroid (astroids gstate) ++
        [uncurry translate (positionPlayer (player gstate)) $ rotate deg ship]
    )
    where
        (x,y) = velocityPlayer (player gstate)
        deg = radToDeg (argV (y,x))


getTime :: GameState -> Float
getTime = elapsedTime