module Enemy where

import Model
import Imports
import Collissions
import Hits
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath
 
enemyPicture :: Picture
enemyPicture = pictures
    [
        color white    $ scale 4 1.5    $ circleSolid 10,
        color white    $ translate 0 10 $ scale 1 1.5 $ circleSolid 15
    ]

-- Every frame, it checks if it needs to spawn an enemy.
-- From 100 seconds it spawns an enemy every 20 seconds.
stepEnemyState :: [Enemy] -> Float -> GameState -> Velocity -> [Enemy]
stepEnemyState [] _ gstate v
    | null (enemy gstate) && timeSpan = [initialEnemy {positionEnemy = v}]
    | otherwise                       = []
    where
        elapsTime = elapsedTime gstate
        timeSpan = round elapsTime `mod` 20 == 0 && round elapsTime > 100

-- It always updates the position, but every other second it reloads
stepEnemyState (enemy:_) time gstate _
    | odd (round (elapsedTime gstate)) = [enemy {positionEnemy = newPos, velocityEnemy = newVel, reloading = False} | not (enemyShot enemy gstate)]
    | otherwise                        = [enemy {positionEnemy = newPos, velocityEnemy = newVel}                    | not (enemyShot enemy gstate)]
    where
        -- The enemy gets updated by setting the velocity in the direction of the player.
        pos    = positionEnemy enemy
        vel    = velocityEnemy enemy
        newPos = pos PMath.+ vel
        newVel = if magV diff <= 0.1 then (0, 0) else normalizeV diff
        diff   = positionPlayer (player gstate) PMath.- pos

-- If the enemy is shot
enemyShot :: Enemy -> GameState -> Bool
enemyShot enemy gstate = enemyShotBullets enemy buls
    where
        buls = bullets gstate

-- If the enemy is shot but using recursion
enemyShotBullets :: Enemy -> [Bullet] -> Bool
enemyShotBullets _ [] = False
enemyShotBullets e (b:bs)
    | pointInEnemy (positionBullet b) e && not (enemyBullet b) = True
    | otherwise = enemyShotBullets e bs


shootEnemyBullet :: GameState -> GameState
shootEnemyBullet gstate
    -- if there is an enemy, every other second it shoots a bullet if its not reloading.
    | not (null (enemy gstate)) =
        if even (round (elapsedTime gstate)) && not (reloading $ head (enemy gstate))
            then gstate {bullets = newBullet : bullets gstate, enemy = [(head (enemy gstate)) {reloading = True}]}
        else gstate
    | otherwise = gstate
            where
                newBullet = Bullet {positionBullet = pos, velocityBullet = vel, enemyBullet = True}
                pos       = positionEnemy $ head (enemy gstate)
                vel       = 200 PMath.* normalizeV (positionPlayer (player gstate) PMath.- pos)