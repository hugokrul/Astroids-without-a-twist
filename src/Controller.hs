-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import View
import Graphics.Gloss
import Ship
import Bullet
import Astroid
import Graphics.Gloss.Interface.IO.Game
-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = case playPauseGameOver gstate of
    Play -> do return $ gstate { elapsedTime = elapsedTime gstate + secs }
    Pause -> return gstate
    GameOver -> return gstate
    

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey KeyUp) Down _ _) gstate = gstate { player = (player gstate) { positionPlayer = moveForward gstate } }
-- inputKey (EventKey (SpecialKey KeyRight) Down _ _) gstate = gstate { player = rotateShip gstate 10 }
-- inputKey (EventKey (SpecialKey KeyLeft) Down _ _) gstate = gstate { player = rotateShip gstate (-10) }
inputKey (EventKey (SpecialKey KeyEsc) Down _ _) gstate = initialState
-- inputKey (EventKey (SpecialKey KeySpace) Down _ _) gstate = fireBullet gstate
inputKey _ gstate = gstate -- Otherwise keep the same

-- fireBullet :: GameState -> GameState
-- fireBullet gstate = gstate { bullets = bullet : bullets}
--     where
--         bullet = Bullet position 200 direction lifespan
--         position = positionPlayer $ player gstate
--         direction = directionBullet $ player gstate
--         lifespan = elapsedTime gstate

-- -- This function returns the world with an updated angle in which the moveForward function will move
-- rotateShip :: GameState -> Int -> World
-- rotateShip gstate speed = Play (
--     Player
--         pos
--         vel
--         acc
--         (dirAngle + fromIntegral speed)
--     )
--     bullets
--     astroids
--         where
--             dirAngle = giveDir (world gstate)
--             vel = giveVelocity (world gstate)
--             acc@(accX, accY) = giveAcceleration (world gstate)
--             pos@(posX, posY) = givePIS (world gstate)
--             bullets = getBullets $ world gstate
--             astroids = getAstroids $ world gstate



-- This function adds the direction to the current position, moving it to the front of which the ship is looking
moveForward :: GameState -> PointInSpace
moveForward gstate = newPos
        where
            oldPos@(x, y) = positionPlayer (player gstate)
            vel = velocityPlayer (player gstate)
            dirAngleDeg = directionPlayer (player gstate)
            dirAngleRad = dirAngleDeg*(pi/180)
            (dirX, dirY) = (sin dirAngleRad, cos dirAngleRad)
            newPos = (x+dirX*vel, y+dirY*vel)