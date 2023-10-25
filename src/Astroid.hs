module Astroid where

import Graphics.Gloss
import Model
import Imports
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath


bigAstroid :: Picture
bigAstroid = scale 2 2 $ color white $ polygon [(17, 0), (33, -6), (32, -22), (23, -32), (12, -26), (0, -24), (2, -13), (1, -5), (7, 0)]

mediumAstroid :: Picture
mediumAstroid = color white $ polygon [(17, 0), (33, -6), (32, -22), (23, -32), (12, -26), (0, -24), (2, -13), (1, -5), (7, 0)]

smallAstroid :: Picture
smallAstroid = scale 0.5 0.5 $ color white $ polygon [(17, 0), (33, -6), (32, -22), (23, -32), (12, -26), (0, -24), (2, -13), (1, -5), (7, 0)]

showAstroid :: Astroid -> Picture
showAstroid a = case sizeAstroid a of
    Big -> uncurry translate (positionAstroid a) $ rotate deg bigAstroid
    Medium -> uncurry translate (positionAstroid a) $ rotate deg mediumAstroid
    Small -> uncurry translate (positionAstroid a)$ rotate deg smallAstroid
    where
        pos@(x1, y1) = positionAstroid a
        (x,y) = velocityAstroid a
        deg = radToDeg (argV (y,x))

stepAstroidsState :: [Astroid] -> Float -> [Astroid]
stepAstroidsState [] _ = []
stepAstroidsState (x:xs) time
    | checkDeleteAstroid x = stepAstroidsState xs time
    | otherwise = calculateNextPositionAstroids x time : stepAstroidsState xs time

checkDeleteAstroid :: Astroid -> Bool
checkDeleteAstroid a
    | x < -466 || x > 466 || y > 316 || y < -316 = True
    | otherwise = False
    where
        x = fst $ positionAstroid a
        y = snd $ positionAstroid a

calculateNextPositionAstroids :: Astroid -> Float -> Astroid
calculateNextPositionAstroids a time = a {positionAstroid = newPos}
    where
        pos = positionAstroid a
        vel = velocityAstroid a
        newPos = pos PMath.+ (time PMath.* vel)

bulletInAstroidList :: Bullet -> [Astroid] -> [Maybe (Bullet, Astroid)]
bulletInAstroidList b [] = [Nothing]
bulletInAstroidList b (x:xs)
    | bulletInAstroid b x = Just (b, x) : bulletInAstroidList b xs
    | otherwise = bulletInAstroidList b xs

bulletInAstroid :: Bullet -> Astroid -> Bool
bulletInAstroid b a = pointInBox p0 p1 p2
    where
        p0 = positionBullet b
        p1 = (ax + 66, ay - 66)
        p2 = (ax, ay)
        (ax, ay) = positionAstroid a

deleteMaybes :: [Maybe (Bullet, Astroid)] -> [(Bullet, Astroid)]
deleteMaybes [] = []
deleteMaybes [x]
    | isJust x = [fromJust x]
    | otherwise = []
deleteMaybes (x:xs)
    | isJust x = fromJust x : deleteMaybes xs
    | otherwise = deleteMaybes xs

checkAstroidShot :: GameState -> GameState
checkAstroidShot gstate 
    | null $ bullets gstate = gstate
    | otherwise = gstate { 
                            bullets = bullets gstate \\ map fst sureList ,
                            astroids = astroids gstate \\ map snd sureList
                         }
                         where
                            buls = bullets gstate
                            astr = astroids gstate
                            bulletHitAstroid = checkAstroidsShot' buls astr
                            sureList = deleteMaybes bulletHitAstroid

checkAstroidsShot' :: [Bullet] -> [Astroid] -> [Maybe (Bullet, Astroid)]
checkAstroidsShot' [] [] = [Nothing]
checkAstroidsShot' [x] a = bulletInAstroidList x a
checkAstroidsShot' (x:xs) a = bulletInAstroidList x a ++ checkAstroidsShot' xs a
