module Ship where

import Collissions
import qualified Data.Set as Set
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath
import Hits
import Imports
import Model

ship :: Float -> Picture
ship animationStep =
  pictures
    [ leftLine,
      rightLine,
      middleLine
    ]
  where
    -- width : 35 height: 50
    -- bottomRight = x + 17.5, y - 25
    leftLine   = translate (-9) 0  $ rotate (20 - 50 * animationStep)    $ color white $ rectangleSolid 1 50
    rightLine  = translate 9 0     $ rotate ((-20) + 70 * animationStep) $ color white $ rectangleSolid 1 50
    middleLine = translate 0 (-10) $ rotate (90 - 90 * animationStep)    $ color white $ rectangleSolid 1 25

-- Wraps around the ship if it goes out of bounds.
wrapShipAround :: Player -> Player
wrapShipAround player
  | x < -420 || x > 420 = player {positionPlayer = (-x, y)}
  | y > 260 || y < -260 = player {positionPlayer = (x, -y)}
  | otherwise           = player
  where
    x = fst $ positionPlayer player
    y = snd $ positionPlayer player

-- Checks for a collission if the player is not reviving.
checkCollission :: GameState -> GameState
checkCollission gstate
  | reviving $ player gstate = gstate
  | otherwise = checkCollissionShipEnemy (player gstate) (enemy gstate) $ checkCollissionPlanet (player gstate) (planets gstate) $ checkCollissionAstroid (player gstate) (astroids gstate) gstate

-- Checks if a player is shot
checkPlayerShot :: GameState -> GameState
checkPlayerShot gstate = checkPlayerShot' (bullets gstate) (player gstate) gstate

-- Checks if a player is shot, but using recursion.
checkPlayerShot' :: [Bullet] -> Player -> GameState -> GameState
checkPlayerShot' [] player gstate = gstate
checkPlayerShot' (b : rest) player gstate
  | not (reviving player) && enemyBullet b && pointInShip (positionBullet b) player = gstate {player = initialPlayer {lives = lives player - 1, reviving = True, deathAnimationTime = (elapsedTime gstate + 3.0), deathPosition = positionPlayer (player), deathVelocity = velocityPlayer player}}
  | otherwise = gstate

-- calculates the next position every frame.
stepPlayerState :: Player -> Float -> GameState -> Player
stepPlayerState player time gstate = player {positionPlayer = newPos, accelarationPlayer = newAcc, reviving = newReviving}
  where
    pos = positionPlayer player
    vel@(vx, vy) = velocityPlayer player
    acc@(ax, ay) = accelarationPlayer player

    newAcc = (ax * 0.99, ay * 0.99)
    newPos@(npx, npy) = pos PMath.+ newAcc
    newReviving = not (reviving player && (npx > 50 || npy > 50)) && reviving player -- grace area

-- This makes sure you can hold a key down, it also checks if keys are pressed simultaneously
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

-- Rotates the ship. E.A. rotates the velosity.
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