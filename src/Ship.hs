module Ship where

import qualified Data.Set as Set
import GHC.IO.Encoding (BufferCodec (getState))
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath
import Graphics.Gloss.Data.Vector
import Imports
import Model

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

p1 :: PointInSpace
p1 = (-3.44, -3.83)

p2 :: PointInSpace
p2 = (4.3, 4)

checkDeleteShip :: Player -> Player
checkDeleteShip player
  | x < -420 || x > 420 = player {positionPlayer = (-x, y)}
  | y > 260 || y < -260 = player {positionPlayer = (x, -y)}
  | otherwise = player
  where
    x = fst $ positionPlayer player
    y = snd $ positionPlayer player

checkCollission :: GameState -> GameState
checkCollission gstate
  | reviving $ player gstate = gstate
  | otherwise = checkCollissionPlanet (player gstate) (planets gstate) $ checkCollission' (player gstate) (astroids gstate) gstate

checkCollissionPlanet :: Player -> [Planet] -> GameState -> GameState
checkCollissionPlanet p [] gstate = gstate
checkCollissionPlanet p [planet] gstate
  | checkCollissionShipPlanet p planet = gstate {player = initialStatePlayer {lives = lives p - 1, reviving = True}}
  | otherwise = gstate
checkCollissionPlanet p (planet : rest) gstate
  | checkCollissionShipPlanet p planet = gstate {player = initialStatePlayer {lives = lives p - 1, reviving = True}}
  | otherwise = checkCollissionPlanet p rest gstate

checkCollission' :: Player -> [Astroid] -> GameState -> GameState
checkCollission' p [] gstate = gstate
checkCollission' p [a] gstate
  | checkCollissionShipAstroid p a = gstate {player = initialStatePlayer {lives = lives p - 1, reviving = True}}
  | otherwise = gstate
checkCollission' p (a : as) gstate
  | checkCollissionShipAstroid p a = gstate {player = initialStatePlayer {lives = lives p - 1, reviving = True}}
  | otherwise = checkCollission' p as gstate

checkCollissionShipPlanet :: Player -> Planet -> Bool
checkCollissionShipPlanet p planet =
  pointInPlanet (x + 17.5, y - 25) planet
    || pointInPlanet (x - 17.5, y - 25) planet
    || pointInPlanet (x + 17.5, y - 25) planet
    || pointInPlanet (x + 17.5, y + 25) planet
  where
    (x, y) = positionPlayer p

checkCollissionShipAstroid :: Player -> Astroid -> Bool
checkCollissionShipAstroid p a =
  pointInAstroid (x + 17.5, y - 25) a
    || pointInAstroid (x - 17.5, y - 25) a
    || pointInAstroid (x + 17.5, y - 25) a
    || pointInAstroid (x + 17.5, y + 25) a
  where
    (x, y) = positionPlayer p

pointInPlanet :: Point -> Planet -> Bool
pointInPlanet p0 planet = pointInBox p0 p1 p2
  where
    p1 = (planetX + 50, planetY - 50)
    p2 = (planetX - 50, planetY + 50)
    (planetX, planetY) = positionPlanet planet

pointInAstroid :: Point -> Astroid -> Bool
pointInAstroid p0 a = case sizeAstroid a of
  Big -> pointInBox p0 pos1 pos2
    where
      pos2 = (ax - 43.4, ay - 38.3)
      pos1 = (ax + 43, ay + 40)
      (ax, ay) = positionAstroid a
  Medium -> pointInBox p0 pos1 pos2
    where
      pos2 = (ax - 17.2, ay - 19.15)
      pos1 = (ax + 21.5, ay + 20)
      (ax, ay) = positionAstroid a
  Small -> pointInBox p0 pos1 pos2
    where
      pos2 = (ax - 6.88, ay - 7.66)
      pos1 = (ax + 8.6, ay + 8)
      (ax, ay) = positionAstroid a

stepPlayerState :: Player -> Float -> GameState -> Player
stepPlayerState player time gstate = player {positionPlayer = newPos, accelarationPlayer = newAcc, reviving = newReviving}
  where
    pos = positionPlayer player
    vel@(vx, vy) = velocityPlayer player
    acc@(ax, ay) = accelarationPlayer player

    newAcc = ((ax * 0.99), (ay * 0.99))
    newPos@(npx, npy) = pos PMath.+ newAcc
    newReviving = not (reviving player && (npx > 50 || npy > 50)) && reviving player -- grace area

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