module Ship where

import Model
import Imports


ship :: Picture
ship = pictures 
    [
        leftLine,
        rightLine,
        middleLine
    ]
    where
        leftLine    = translate (-9)    0   $ rotate 20     $ color white $ rectangleSolid 1 50
        rightLine   = translate   9     0   $ rotate (-20)  $ color white $ rectangleSolid 1 50
        middleLine  = translate   0  (-10)  $ rotate 90     $ color white $ rectangleSolid 1 25

checkDeleteShip :: GameState -> GameState
checkDeleteShip gstate
    | x < -380 || x > 380 || y > 220 || y < -220 = gstate {playPauseGameOver=GameOver}
    | otherwise = gstate
    where 
        x = fst $ positionPlayer $ player gstate
        y = snd $ positionPlayer $ player gstate

stepPlayerState :: Player -> Float -> Player
stepPlayerState player time = player 
                                {
                                    positionPlayer = 
                                }


-- This function adds the direction to the current position, moving it to the front of which the ship is looking
moveForward :: GameState -> PointInSpace
moveForward gstate = newPos
        where
            (x, y) = positionPlayer (player gstate)
            vel = velocityPlayer (player gstate)
            dirAngleDeg = directionPlayer (player gstate)
            dirAngleRad = dirAngleDeg*(pi/180)
            (dirX, dirY) = (sin dirAngleRad, cos dirAngleRad)
            newPos = positionPlayer mapPlus