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
import Planet

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
                             color white $ translate (-400) 150 $ scale 0.5 0.25 $ text "Press Esc to play again"
                            ]
    
    Start       -> Pictures [
                                color white $ translate (-120) 200 $ scale 0.5 0.25 $ text "Astroids",
                                color white $ translate (-200) 150 $ scale 0.5 0.25 $ text "Witout a twist",
                                color red   $ rectangleWire 150 70,
                                color white $ translate (-70) (-10) $ scale 0.5 0.25 $ text "Start"
                            ]


getPictures :: GameState -> Picture
getPictures gstate = pictures (
        map showBullet (bullets gstate) ++
        map showAstroid (astroids gstate) ++
        map showPlanet (planets gstate) ++
        [uncurry translate (positionPlayer (player gstate)) $ rotate deg ship]
    )
    where
        (x,y) = velocityPlayer (player gstate)
        deg = radToDeg (argV (y,x))


getTime :: GameState -> Float
getTime = elapsedTime