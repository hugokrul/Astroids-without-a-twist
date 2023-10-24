module Bullet where

import Model
import Graphics.Gloss

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