module Ship where

import qualified Data.Set as Set
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath
import Graphics.Gloss.Data.Vector
import Imports
import Model
import Collissions
import GHC.IO.Encoding (BufferCodec(getState))

ship :: Picture
ship =
  pictures
    [ leftLine,
      rightLine,
      middleLine
    ]
  where
    -- width : 35 height: 50
    -- bottomRight = x + 17.5, y - 25
    leftLine = translate (-9) 0 $ rotate 20 $ color white $ rectangleSolid 1 50
    rightLine = translate 9 0 $ rotate (-20) $ color white $ rectangleSolid 1 50
    middleLine = translate 0 (-10) $ rotate 90 $ color white $ rectangleSolid 1 25

checkDeleteShip :: Player -> Player
checkDeleteShip player
  | x < -420 || x > 420 = player { positionPlayer = (-x, y) }
  | y > 260 || y < -260 = player { positionPlayer = (x, -y) }
  | otherwise = player
  where
    x = fst $ positionPlayer player
    y = snd $ positionPlayer player

checkCollission :: GameState -> GameState
checkCollission gstate = checkCollissionPlanet (player gstate) (planets gstate) $ checkCollissionAstroid (player gstate) (astroids gstate) gstate

stepPlayerState :: Player -> Float -> GameState -> Player
stepPlayerState player time gstate = player {positionPlayer = newPos, accelarationPlayer = newAcc}
  where
    pos = positionPlayer player
    vel@(vx, vy) = velocityPlayer player
    acc@(ax, ay) = accelarationPlayer player

    newAcc = ((ax * 0.99), (ay * 0.99))
    newPos = pos PMath.+ newAcc

updatePosition :: GameState -> Player -> Player
updatePosition gstate player
  | up && right = moveForward (player {velocityPlayer = rotateShip player 1}) (elapsedTime gstate)
  | up && left = moveForward (player {velocityPlayer = rotateShip player (-1)}) (elapsedTime gstate)
  | up = moveForward player (elapsedTime gstate)
  | right = player {velocityPlayer = rotateShip player 1}
  | left = player {velocityPlayer = rotateShip player (-1)}
  | otherwise = player
  where
    up = Set.member KeyUp (keySet gstate)
    right = Set.member KeyRight (keySet gstate)
    left = Set.member KeyLeft (keySet gstate)

rotateShip :: Player -> Float -> Vector
rotateShip player speed = newVel
  where
    pos = positionPlayer player
    vel = velocityPlayer player
    newVel = rotateV (-(degToRad (speed * 3))) vel -- multiplier for rotation

-- This function adds the direction to the current position, moving it to the front of which the ship is looking
moveForward :: Player -> Float -> Player
moveForward player time = player {positionPlayer = newPos, accelarationPlayer = newAcc}
  where
    pos = positionPlayer player
    vel = velocityPlayer player
    acc = accelarationPlayer player
    newAcc | magV acc < 10 = acc PMath.+ vel | otherwise = acc -- limit acceleration
    newPos = pos PMath.+ vel