module Bullet where

import Model
import Graphics.Gloss
import Data.Bits (Bits(xor))

mkBullet :: Bullet -> Float -> Picture
mkBullet bullet@(Bullet (x,y) v d t) time = translate posx posy $ color white $ ThickCircle 1 2
    where
        posx = x + dirx*v*(time-t)
        posy = y + diry*v*(time-t)
        (dirx, diry) = (sin dirAngleRad, cos dirAngleRad)
        dirAngleRad = d*(pi/180)

getBulletsPicture :: World -> Float -> [Picture] 
getBulletsPicture (Play _ []) _= []
getBulletsPicture (Play p (bullet:rest)) time = mkBullet bullet time : getBulletsPicture (Play p rest) time

getBullets :: World -> [Bullet]
getBullets (Play _ bullets) = bullets

addBullet :: World -> Bullet -> World
addBullet (Play player bulletList) bullet = Play player ([bullet] ++ bulletList)
