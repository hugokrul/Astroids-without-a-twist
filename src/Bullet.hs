module Bullet where

import Model

import Graphics.Gloss
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


--uncurry translate (positionPlayer (player gstate)) $ rotate (directionPlayer (player gstate)) ship,

-- mkBullet :: Bullet -> Float -> Picture
-- mkBullet bullet@(Bullet (x,y) v d t) time = translate posx posy $ color white $ ThickCircle 1 2
--     where
--         posx = x + dirx*v*(time-t)
--         posy = y + diry*v*(time-t)
--         (dirx, diry) = (sin dirAngleRad, cos dirAngleRad)
--         dirAngleRad = d*(pi/180)

-- getBulletsPicture :: GameState -> Float -> [Picture] 
-- getBulletsPicture (Play _ [] _) _= []
-- getBulletsPicture (Play p (bullet:rest) a) time = mkBullet bullet time : getBulletsPicture (Play p rest a) time

-- checkDeleteBullet :: World -> Float -> World
-- checkDeleteBullet (Play p [] a) time = Play p [] a
-- checkDeleteBullet (Play p list@(bullet:rest) a) time
--     | (time - getBulletTime bullet) >= 3 = Play p rest a
--     | otherwise = Play p list a