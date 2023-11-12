module Astroid where

import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath
import Imports
import Model
import Hits

polygonPoints :: [(Float, Float)]
polygonPoints = [(-3.44,2.84), (-1,4), (1.84,2.66), (3.14,1.16), (4.3,-0.38), (3.2,-1.5), (2.06,-2.8), (0.54,-3.78), (-2.24,-3.82), (-3.2,-1.98), (-2.62,-0.44), (-3.46,0.82)]

bigAstroid :: Picture
bigAstroid    = scale 10 10 $ color white $ polygon polygonPoints

mediumAstroid :: Picture
mediumAstroid = scale 5 5   $ color white $ polygon polygonPoints

smallAstroid :: Picture
smallAstroid  = scale 2 2   $ color white $ polygon polygonPoints

-- Show the different kinds of astroids. 
showAstroid :: Astroid -> Picture
showAstroid a = case sizeAstroid a of
  Big    -> uncurry translate pos $ rotate deg bigAstroid
  Medium -> uncurry translate pos $ rotate deg mediumAstroid
  Small  -> uncurry translate pos $ rotate deg smallAstroid
  where
    pos@(x1, y1) = positionAstroid a
    (x, y) = velocityAstroid a
    deg = radToDeg (argV (y, x))

-- This function repeats every second.
stepAstroidsState :: [Astroid] -> Float -> GameState -> [Velocity] -> [Astroid]
stepAstroidsState xs time gamestate randomVels
  = foldr
      -- For every astroid it calculates the next position. 
      (\ astroid -> (:) (calculateNextPositionAstroids astroid time))
      -- If there are no astroids in gstate and the time is a multiple of 10, it adds 4 astroids to the astroid list. 
      -- This way it spawns 4 astroids every 10 seconds if all astroids are destroid.
      (if null (astroids gamestate) && round (elapsedTime gamestate) `mod` 10 == 0 then
           [Astroid
              {positionAstroid = (0, 300), velocityAstroid = head randomVels,
               sizeAstroid = Big},
            Astroid
              {positionAstroid = (0, -300), velocityAstroid = randomVels !! 1,
               sizeAstroid = Big},
            Astroid
              {positionAstroid = (400, 0), velocityAstroid = randomVels !! 2,
               sizeAstroid = Big},
            Astroid
              {positionAstroid = (-400, 0), velocityAstroid = randomVels !! 3,
               sizeAstroid = Big}
            ]
       else
           [])
      xs

-- Wrap the astroid around if it goes out of bouds. 
checkWrapAroundAstroid :: Astroid -> Astroid
checkWrapAroundAstroid a
    | x > 500 || x < -500 = a { positionAstroid = (-x, y) }
    | y > 350 || y < -350 = a { positionAstroid = (x, -y) }
    | otherwise           = a
    where
        (x, y) = positionAstroid a

-- Calculates the next position every frame. speeds the astroids up if there big, medium or small. 
calculateNextPositionAstroids :: Astroid -> Float -> Astroid
calculateNextPositionAstroids a time = case sizeAstroid a of
    Big     -> checkWrapAroundAstroid a {positionAstroid = newPos}
                where
                    pos = positionAstroid a
                    vel = velocityAstroid a
                    newPos = pos PMath.+ (2 PMath.* (time PMath.* vel))
    Medium  -> checkWrapAroundAstroid a {positionAstroid = newPos}
                where
                    pos = positionAstroid a
                    vel = velocityAstroid a
                    newPos = pos PMath.+ (5 PMath.* (time PMath.* vel))

    Small   -> checkWrapAroundAstroid a {positionAstroid = newPos}
                where
                    pos = positionAstroid a
                    vel = velocityAstroid a
                    newPos = pos PMath.+ (10 PMath.* (time PMath.* vel))

-- Goes through every astroid in the game to see if the given bullet hits the astroid. and if the bullet is a bullet from the player, not the enemy.
bulletInAstroidList :: Bullet -> [Astroid] -> [Maybe (Bullet, Astroid)]
bulletInAstroidList b [] = [Nothing]
bulletInAstroidList b (x : xs)
  | not (enemyBullet b) && pointInAstroid (positionBullet b) x = Just (b, x) : bulletInAstroidList b xs
  | otherwise                                                  = bulletInAstroidList b xs

-- Deletes the maybes for the bulletInAstroidList function and for the smallerAstroid function.
deleteMaybes :: [Maybe a] -> [a]
deleteMaybes [] = []
deleteMaybes [x]
  | isJust x  = [fromJust x]
  | otherwise = []
deleteMaybes (x : xs)
  | isJust x  = fromJust x : deleteMaybes xs
  | otherwise = deleteMaybes xs

-- Returns the game state where every astroid is deleted if it was hit by a bullet.
checkAstroidShot :: GameState -> GameState
checkAstroidShot gstate
  | null $ bullets gstate = gstate
  | otherwise             =
                            gstate
                              { bullets = bullets gstate \\ map fst sureList,
                                astroids = (astroids gstate \\ map snd sureList) ++ deleteMaybes (makeAstroidsSmaller onlyAstroids)
                              }
  where
    buls             = bullets gstate
    astr             = astroids gstate
    onlyAstroids     = map snd sureList
    bulletHitAstroid = checkAstroidsShot' buls astr
    sureList         = deleteMaybes bulletHitAstroid -- if there is an astroid hit: sureList = (Bullet, Astroid)

-- If an astroid is hit, the astroid has to be smaller. 
makeAstroidsSmaller :: [Astroid] -> [Maybe Astroid]
makeAstroidsSmaller = concatMap smallerAstroid

-- Checks if it is possible to split an astroid, if so it gives back a two new astroids with new velocities, if not it gives back Nothing
smallerAstroid :: Astroid -> [Maybe Astroid]
smallerAstroid a = case sizeAstroid a of
  Big    -> [Just a {positionAstroid = pos1, velocityAstroid = vel, sizeAstroid = Medium}, Just a {positionAstroid = pos2, sizeAstroid = Medium}]
    where
      pos1 = (-10, -10) PMath.+ positionAstroid a
      pos2 = (10, 10)   PMath.+ positionAstroid a
      vel  = (7, 7)     PMath.+ velocityAstroid a
  Medium -> [Just a {positionAstroid = pos1, velocityAstroid = vel, sizeAstroid = Small}, Just a {positionAstroid = pos2, sizeAstroid = Small}]
    where
      pos1 = (-10, -10) PMath.+ positionAstroid a
      pos2 = (10, 10)   PMath.+ positionAstroid a
      vel  = (-5, 5)    PMath.+ velocityAstroid a
  _      -> [Nothing]

-- Checks for every bullet in the game if it is intersects with an astroid. 
checkAstroidsShot' :: [Bullet] -> [Astroid] -> [Maybe (Bullet, Astroid)]
checkAstroidsShot' []       [] = [Nothing]
checkAstroidsShot' [x]      a  = bulletInAstroidList x a
checkAstroidsShot' (x : xs) a  = bulletInAstroidList x a ++ checkAstroidsShot' xs a
