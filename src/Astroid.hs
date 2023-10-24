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

bigAstroidHitBox :: Astroid -> [Point]
-- topleft, topright, bottomright, bottomleft
bigAstroidHitBox a = [pos, (x+66, y), (x+66, y-64), (x, y-64)]
    where
        pos@(x, y) = positionAstroid a

mediumAstroidHitBox :: Astroid -> [Point]
-- topleft, topright, bottomright, bottomleft
mediumAstroidHitBox a = [pos, (x+33, y), (x+33, y-32), (x, y-32)]
    where
        pos@(x, y) = positionAstroid a

smallAstroidHitBox :: Astroid -> [Point]
-- topleft, topright, bottomright, bottomleft
smallAstroidHitBox a = [pos, (x+16.5, y), (x+16.5, y-16), (x, y-16)]
    where
        pos@(x, y) = positionAstroid a


showAstroid :: Astroid -> Picture
showAstroid a = case sizeAstroid a of 
    Big -> pictures [uncurry translate (positionAstroid a) $ rotate deg bigAstroid]
    Medium -> uncurry translate (positionAstroid a) $ rotate deg mediumAstroid
    Small -> uncurry translate (positionAstroid a)$ rotate deg smallAstroid
    where
        (x,y) = velocityAstroid a
        deg = radToDeg (argV(y,x))

stepAstroidsState :: [Astroid] -> Float -> [Astroid]
stepAstroidsState [] _ = []
stepAstroidsState (x:xs) time 
    | checkDeleteAstroid x = stepAstroidsState xs time
    | otherwise = calculateNextPositionAstroids x time : stepAstroidsState xs time

checkDeleteAstroid :: Astroid -> Bool
checkDeleteAstroid a
    | x < -400 || x > 400 || y > 250 || y < -250 = True
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
