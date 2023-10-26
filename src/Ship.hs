module Ship where

import Model
import Imports
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath

ship :: Picture
ship = pictures
    [
        leftLine,
        rightLine,
        middleLine
    ]
    -- width : 35 height: 50
    -- bottomRight = x + 17.5, y - 25
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

checkCollission :: GameState -> GameState
checkCollission gstate = checkCollission' (player gstate) (astroids gstate) gstate

checkCollission' :: Player -> [Astroid] -> GameState -> GameState
checkCollission' p [] gstate = gstate
checkCollission' p [a] gstate 
    | checkCollissionShipAstroid p a = gstate { player = p { positionPlayer = (0, 0), lives = lives p - 1}}
    | otherwise = gstate
checkCollission' p (a:as) gstate
    | checkCollissionShipAstroid p a = gstate { player = p { positionPlayer = (0, 0), lives = lives p - 1}}
    | otherwise = checkCollission' p as gstate


checkCollissionShipAstroid :: Player -> Astroid -> Bool
checkCollissionShipAstroid p a =
    pointInAstroid (x + 17.5, y - 25) a ||
    pointInAstroid (x - 17.5, y - 25) a ||
    pointInAstroid (x + 17.5, y - 25) a ||
    pointInAstroid (x + 17.5, y + 25) a
    where
        (x, y) = positionPlayer p

pointInAstroid :: Point -> Astroid -> Bool
pointInAstroid p0 a = case sizeAstroid a of 
    Big -> pointInBox p0 p1 p2
        where
            p1 = (ax, ay)
            p2 = (ax+66, ay - 66)
            (ax, ay) = positionAstroid a
    Medium -> pointInBox p0 p1 p2 
        where
            p1 = (ax, ay)
            p2 = (ax+33, ay - 33)
            (ax, ay) = positionAstroid a
    Small -> pointInBox p0 p1 p2 
        where
            p1 = (ax, ay)
            p2 = (ax+17, ay - 17)
            (ax, ay) = positionAstroid a

stepPlayerState :: Player -> Float -> Player
stepPlayerState player time = player

rotateShip :: Player -> Float -> Vector
rotateShip player speed = newVel
        where
            pos = positionPlayer player
            vel = velocityPlayer player
            newVel = rotateV (-(degToRad speed)) vel 

-- This function adds the direction to the current position, moving it to the front of which the ship is looking
moveForward :: Player -> Float -> Player
moveForward player time = player { positionPlayer = newPos, accelarationPlayer = vel}
        where
            pos = positionPlayer player
            vel = velocityPlayer player
            newPos = pos PMath.+ vel