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

-- Shows different screens for different gamestates.
viewPure :: GameState -> Picture
viewPure gstate = case playPauseGameOver gstate of
  Play ->
    pictures
      [ getPictures gstate,
        color white $ translate (-400) 220 $ scale 0.3 0.2 $ text  $ "Lives:"     ++ show (lives $ player gstate),
        color white $ translate (-200) 220 $ scale 0.2 0.15 $ text $ "Highscore:" ++ show (round' (highScore gstate) 2),
        color white $ translate 0 220      $ scale 0.2 0.15 $ text $ "Score:"     ++ show (round' (score gstate) 2)
      ]
  Pause ->
    pictures
      [ getPictures gstate,-- Shows everything (keep in mind it doesnt update anything)
        color white $ translate (-400) 220 $ scale 0.3 0.2  $ text $ "Lives:"     ++ show (lives $ player gstate),
        color white $ translate (-200) 220 $ scale 0.2 0.15 $ text $ "Highscore:" ++ show (round' (highScore gstate) 2),
        color white $ translate 0 220      $ scale 0.2 0.15 $ text $ "Score:"     ++ show (round' (score gstate) 2),
        color white $ translate (-400) 150 $ scale 0.5 0.25 $ text   "Paused"
      ]
  GameOver ->
    Pictures
      [ color white $ translate (-400) 200   $ scale 0.5 0.25 $ text   "Game Over",
        color white $ translate (-400) 150   $ scale 0.5 0.25 $ text   "Press Esc to play again",
        color white $ translate (-150) (-50) $ scale 0.3 0.2  $ text $ "Highscore:" ++ show (highScore gstate) -- The high score
      ]
  Start ->
    Pictures
      [ color white $ translate (-120) 200  $ scale 0.5 0.25 $ text "Astroids",
        color white $ translate (-200) 150  $ scale 0.5 0.25 $ text "Witout a twist",
        color red   $ rectangleWire 150 70, -- For the button
        color white $ translate (-70) (-10) $ scale 0.5 0.25 $ text "Start"
      ]

getPictures :: GameState -> Picture
getPictures gstate =
  pictures
    (      map showBullet (bullets gstate) -- Shows all bullets
        ++ map showAstroid (astroids gstate) -- Shows all astroids
        ++ map showPlanet (planets gstate) -- Shows all planets
        ++ [uncurry translate (positionPlayer (player gstate)) $ rotate deg (ship 0)]
        ++ ([uncurry translate (deathPosition (player gstate)) $ rotate degdeath shipDeathPicture | timeDiff > 0]) -- If a player dies, it shows the death animation
        ++ ([uncurry translate (positionEnemy (head (enemy gstate))) enemyPicture | not (null (enemy gstate))])
    )
  where
    (x, y)           = velocityPlayer (player gstate)
    deg              = radToDeg (argV (y, x))
    (xd, yd)         = deathVelocity (player gstate)
    degdeath         = radToDeg (argV (yd, xd))
    (enemyX, enemyY) = velocityEnemy (head (enemy gstate))
    degE             = radToDeg (argV (enemyY, enemyX))

    timeDiff         = max (deathAnimationTime (player gstate) - elapsedTime gstate) 0
    animationStep    = if timeDiff > 0 then (3.0 - timeDiff) / 3.0 else 0 -- 2.0 = animationTime
    shipDeathPicture = ship animationStep

-- Rounds an integer to a specific position.
round' :: Float -> Int -> Float
round' x n = fromIntegral (floor (x * (10 ^ n))) / 10 ^ n