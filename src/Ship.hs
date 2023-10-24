module Ship where

import Model
import Imports
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath
import Graphics.Gloss (Vector)


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

checkDeleteShip :: Player -> Player
checkDeleteShip player
    | x < -380 || x > 380 || y > 220 || y < -220 = player { positionPlayer = (0, 0), lives = lives player - 1}
    | otherwise = player
    where
        x = fst $ positionPlayer player
        y = snd $ positionPlayer player

stepPlayerState :: Player -> Float -> Player
stepPlayerState player time = player

-- This function adds the direction to the current position, moving it to the front of which the ship is looking
moveForward :: Player -> Float -> Player
moveForward player time = player { positionPlayer = newPos, accelarationPlayer = vel}
        where
            pos = positionPlayer player
            vel = velocityPlayer player
            newPos = pos PMath.+ vel