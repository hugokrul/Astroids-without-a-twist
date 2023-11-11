{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use :" #-}

-- | This module defines how to turn
--   the game state into a picture
module View where

import Astroid
import Bullet
import Enemy
import Imports
import Model
import Planet
import Ship

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case playPauseGameOver gstate of
  Play ->
    pictures
      [ getPictures gstate,
        color white $ translate (-400) 220 $ scale 0.3 0.2 $ text $ "Lives:" ++ show (lives $ player gstate),
        color white $ translate (-200) 220 $ scale 0.2 0.15 $ text $ "Highscore:" ++ show (round' (highScore gstate) 2),
        color white $ translate (0) 220 $ scale 0.2 0.15 $ text $ "Score:" ++ show (round' (score gstate) 2)
      ]
  Pause ->
    pictures
      [ getPictures gstate,
        color white $ translate (-400) 220 $ scale 0.3 0.2 $ text $ "Lives:" ++ show (lives $ player gstate),
        color white $ translate (-200) 220 $ scale 0.2 0.15 $ text $ "Highscore:" ++ show (round' (highScore gstate) 2),
        color white $ translate (0) 220 $ scale 0.2 0.15 $ text $ "Score:" ++ show (round' (score gstate) 2),
        color white $ translate (-400) 150 $ scale 0.5 0.25 $ text "Paused"
      ]
  GameOver ->
    Pictures
      [ color white $ translate (-400) 200 $ scale 0.5 0.25 $ text "Game Over",
        color white $ translate (-400) 150 $ scale 0.5 0.25 $ text "Press Esc to play again",
        color white $ translate (-150) (-50) $ scale 0.3 0.2 $ text $ "Highscore:" ++ show (highScore gstate)
      ]
  Start ->
    Pictures
      [ color white $ translate (-120) 200 $ scale 0.5 0.25 $ text "Astroids",
        color white $ translate (-200) 150 $ scale 0.5 0.25 $ text "Witout a twist",
        color red $ rectangleWire 150 70,
        color white $ translate (-70) (-10) $ scale 0.5 0.25 $ text "Start"
      ]

getPictures :: GameState -> Picture
getPictures gstate =
  pictures
    ( map showBullet (bullets gstate)
        ++ map showAstroid (astroids gstate)
        ++ map showPlanet (planets gstate)
        ++ [uncurry translate (positionPlayer (player gstate)) $ rotate deg (ship 0)]
        ++ ( if (timeDiff > 0)
               then ([uncurry translate (deathPosition (player gstate)) $ rotate degdeath shipDeathPicture])
               else []
           )
        ++ ([uncurry translate (positionEnemy (head (enemy gstate))) enemyPicture | not (null (enemy gstate))])
    )
  where
    (x, y) = velocityPlayer (player gstate)
    deg = radToDeg (argV (y, x))
    (xd, yd) = deathVelocity (player gstate)
    degdeath = radToDeg (argV (yd, xd))
    (enemyX, enemyY) = velocityEnemy (head (enemy gstate))
    degE = radToDeg (argV (enemyY, enemyX))

    timeDiff = max (deathAnimationTime (player gstate) - elapsedTime gstate) 0
    animationStep = if (timeDiff > 0) then (3.0 - timeDiff) / 3.0 else 0 -- 2.0 = animationTime
    shipDeathPicture = ship animationStep

getTime :: GameState -> Float
getTime = elapsedTime

round' :: Float -> Int -> Float
round' x n = (fromIntegral (floor (x * t))) / t
  where
    t = 10 ^ n