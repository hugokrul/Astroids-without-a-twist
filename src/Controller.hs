-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import View
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = return $ gstate { elapsedTime = elapsedTime gstate + secs }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey KeyUp) _ _ _) gstate = gstate { world = moveForward gstate }
inputKey (EventKey (SpecialKey KeyRight) _ _ _) gstate = gstate { world = rotateShip gstate 5 }
inputKey (EventKey (SpecialKey KeyLeft) _ _ _) gstate = gstate { world = rotateShip gstate (-5) }
inputKey (EventKey (SpecialKey KeyEsc) _ _ _) gstate = initialState
inputKey _ gstate = gstate -- Otherwise keep the same

rotateShip :: GameState -> Int -> World
rotateShip gstate speed = Play (
    Player 
        pos 
        vel 
        acc 
        (dirAngle + fromIntegral speed)
    )
        where   
            dirAngle = giveDir (world gstate)
            vel = giveVelocity (world gstate)
            acc@(accX, accY) = giveAcceleration (world gstate)
            pos@(posX, posY) = givePIS (world gstate)

moveForward :: GameState -> World
moveForward gstate = Play (
        Player 
            (posX + dirX*vel, posY + dirY*vel) 
            vel
            acc
            dirAngle
    )     
        where
            pos@(posX, posY) = givePIS (world gstate)
            vel = giveVelocity (world gstate)
            acc@(accX, accY) = giveAcceleration (world gstate)
            (dirX, dirY) = (sin (dirAngle*(pi/180)), cos (dirAngle*(pi/180)))
            dirAngle = giveDir (world gstate)