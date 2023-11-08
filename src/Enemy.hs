module Enemy where

import Model
import Imports
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath


enemyPicture :: Picture
enemyPicture = pictures
    [
        color white $ scale 4 1.5 $ circleSolid 10,
        translate 0 10 $ color white $ scale 1 1.5 $ circleSolid 15
    ]

stepEnemyState :: [Enemy] -> Float -> GameState -> [Enemy]
stepEnemyState [] _ gstate
    | null (enemy gstate) && timeSpan = [testEnemy]
    | otherwise = []
    where
        elapsTime = elapsedTime gstate
        timeSpan = round elapsTime `mod` 100 == 0 && round elapsTime > 1

stepEnemyState (enemy:_) time gstate 
    | odd (round (elapsedTime gstate)) = [enemy {positionEnemy = newPos, velocityEnemy = newVel, reloading = False} {-| not (enemyShot gstate)-}]
    | otherwise = [enemy {positionEnemy = newPos, velocityEnemy = newVel}]
    where
        pos = positionEnemy enemy
        vel@(vx, vy) = velocityEnemy enemy
        newPos = pos PMath.+ vel
        newVel = if magV diff <= 0.1 then (0, 0) else normalizeV diff
        diff = positionPlayer (player gstate) PMath.- pos

shootEnemyBullet :: GameState -> GameState
shootEnemyBullet gstate
    | not (null (enemy gstate)) = 
        if even (round (elapsedTime gstate)) && not (reloading $ head (enemy gstate)) 
            then gstate {bullets = newBullet : bullets gstate, enemy = [(head (enemy gstate)) {reloading = True}]} 
            else gstate
    | otherwise = gstate
            where
                newBullet = Bullet {positionBullet = pos, velocityBullet = vel, enemyBullet = True}
                pos = positionEnemy $ head (enemy gstate)
                vel = 200 PMath.* normalizeV (positionPlayer (player gstate) PMath.- pos)