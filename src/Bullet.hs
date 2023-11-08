module Bullet where

import Model

import Imports
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath

bullet :: Picture
bullet = color white $ ThickCircle 1 2

showBullet :: Bullet -> Picture
showBullet b = uncurry translate (positionBullet b) bullet

stepBulletsState :: [Bullet] -> Float -> [Bullet]
stepBulletsState [] _ = []
stepBulletsState (x:xs) time 
    | checkDeleteBullet x = stepBulletsState xs time
    | otherwise = calculateNextPosition x time : stepBulletsState xs time

checkDeleteBullet :: Bullet -> Bool
checkDeleteBullet bullet
    | x < -400 || x > 400 || y > 250 || y < -250 = True
    | otherwise = False
    where
        x = fst $ positionBullet bullet
        y = snd $ positionBullet bullet

calculateNextPosition :: Bullet -> Float -> Bullet
calculateNextPosition bullet time = bullet {positionBullet = newPos}
    where 
        pos = positionBullet bullet
        vel = velocityBullet bullet
        newPos = pos PMath.+ (time PMath.* vel)

fireBullet :: GameState -> [Bullet]
fireBullet gstate = bullet : bullets gstate
    where
        bullet = Bullet position (2000 PMath.* vel) False
        position = positionPlayer $ player gstate
        vel = velocityPlayer $ player gstate