module Astroid where

import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath
import Imports
import Model

bigAstroid :: Picture
bigAstroid = scale 2 2 $ color white $ polygon [(17, 0), (33, -6), (32, -22), (23, -32), (12, -26), (0, -24), (2, -13), (1, -5), (7, 0)]

mediumAstroid :: Picture
mediumAstroid = color white $ polygon [(17, 0), (33, -6), (32, -22), (23, -32), (12, -26), (0, -24), (2, -13), (1, -5), (7, 0)]

smallAstroid :: Picture
smallAstroid = scale 0.5 0.5 $ color white $ polygon [(17, 0), (33, -6), (32, -22), (23, -32), (12, -26), (0, -24), (2, -13), (1, -5), (7, 0)]

showAstroid :: Astroid -> Picture
showAstroid a = case sizeAstroid a of
  Big -> uncurry translate pos $ rotate deg bigAstroid
  Medium -> uncurry translate pos $ rotate deg mediumAstroid
  Small -> uncurry translate pos $ rotate deg smallAstroid
  where
    pos@(x1, y1) = positionAstroid a
    (x, y) = velocityAstroid a
    deg = radToDeg (argV (y, x))

stepAstroidsState :: [Astroid] -> Float -> GameState -> [Velocity] -> [Astroid]
stepAstroidsState xs time gamestate randomVels
  = foldr
      (\ x -> (:) (calculateNextPositionAstroids x time))
      (if null (astroids gamestate) then
           [Astroid
              {positionAstroid = (0, 300), velocityAstroid = head randomVels,
               sizeAstroid = Big},
            Astroid
              {positionAstroid = (0, - 300), velocityAstroid = randomVels !! 1,
               sizeAstroid = Big},
            Astroid
              {positionAstroid = (400, 0), velocityAstroid = randomVels !! 2,
               sizeAstroid = Big},
            Astroid
              {positionAstroid = (400, 0), velocityAstroid = randomVels !! 3,
               sizeAstroid = Big}
            ]
       else
           [])
      xs

checkWrapAroundAstroid :: Astroid -> Astroid
checkWrapAroundAstroid a
    | x > 500 || x < -500 = a { positionAstroid = (-x, y) }
    | y > 350 || y < -350 = a { positionAstroid = (x, -y) }
    | otherwise = a
    where
        (x, y) = positionAstroid a

calculateNextPositionAstroids :: Astroid -> Float -> Astroid
calculateNextPositionAstroids a time = checkWrapAroundAstroid a {positionAstroid = newPos}
    where
        pos = positionAstroid a
        vel = velocityAstroid a
        newPos = pos PMath.+ (time PMath.* vel)

bulletInAstroidList :: Bullet -> [Astroid] -> [Maybe (Bullet, Astroid)]
bulletInAstroidList b [] = [Nothing]
bulletInAstroidList b (x : xs)
  | bulletInAstroid b x = Just (b, x) : bulletInAstroidList b xs
  | otherwise = bulletInAstroidList b xs

bulletInAstroid :: Bullet -> Astroid -> Bool
bulletInAstroid b a = case sizeAstroid a of
  Big -> pointInBox p0 p1 p2
    where
      p0 = positionBullet b
      p2 = (ax, ay)
      p1 = (ax + 66, ay - 66)
      (ax, ay) = positionAstroid a
  Medium -> pointInBox p0 p1 p2
    where
      p0 = positionBullet b
      p2 = (ax, ay)
      p1 = (ax + 33, ay - 33)
      (ax, ay) = positionAstroid a
  Small -> pointInBox p0 p1 p2
    where
      p0 = positionBullet b
      p2 = (ax, ay)
      p1 = (ax + 17, ay - 17)
      (ax, ay) = positionAstroid a

deleteMaybes :: [Maybe a] -> [a]
deleteMaybes [] = []
deleteMaybes [x]
  | isJust x = [fromJust x]
  | otherwise = []
deleteMaybes (x : xs)
  | isJust x = fromJust x : deleteMaybes xs
  | otherwise = deleteMaybes xs

checkAstroidShot :: GameState -> GameState
checkAstroidShot gstate
  | null $ bullets gstate = gstate
  | otherwise =
      gstate
        { bullets = bullets gstate \\ map fst sureList,
          astroids = (astroids gstate \\ map snd sureList) ++ deleteMaybes (makeAstroidsSmaller onlyAstroids)
        }
  where
    buls = bullets gstate
    astr = astroids gstate
    onlyAstroids = map snd sureList
    bulletHitAstroid = checkAstroidsShot' buls astr
    sureList = deleteMaybes bulletHitAstroid

makeAstroidsSmaller :: [Astroid] -> [Maybe Astroid]
makeAstroidsSmaller = concatMap smallerAstroid

smallerAstroid :: Astroid -> [Maybe Astroid]
smallerAstroid a = case sizeAstroid a of
  Big -> [Just a {positionAstroid = pos1, velocityAstroid = vel, sizeAstroid = Medium}, Just a {positionAstroid = pos2, sizeAstroid = Medium}]
    where
      pos1 = (-10, -10) PMath.+ positionAstroid a
      pos2 = (10, 10) PMath.+ positionAstroid a
      vel = (7, 7) PMath.+ velocityAstroid a
  Medium -> [Just a {positionAstroid = pos1, velocityAstroid = vel, sizeAstroid = Small}, Just a {positionAstroid = pos2, sizeAstroid = Small}]
    where
      pos1 = (-10, -10) PMath.+ positionAstroid a
      pos2 = (10, 10) PMath.+ positionAstroid a
      vel = (-5, 5) PMath.+ velocityAstroid a
  _ -> [Nothing]

checkAstroidsShot' :: [Bullet] -> [Astroid] -> [Maybe (Bullet, Astroid)]
checkAstroidsShot' [] [] = [Nothing]
checkAstroidsShot' [x] a = bulletInAstroidList x a
checkAstroidsShot' (x : xs) a = bulletInAstroidList x a ++ checkAstroidsShot' xs a
