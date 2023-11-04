module Planet where

import Imports
import Model
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath

planet :: Picture
planet = color white $ thickCircle 50 100

showPlanet :: Planet -> Picture
showPlanet p = uncurry translate (positionPlanet p) planet

stepPlanetsState :: [Planet] -> Float -> GameState -> [Velocity] -> [Planet]
stepPlanetsState [] _ gstate vels
    | null (planets gstate) && timeSpan = [Planet {positionPlanet = (400, -350), velocityPlanet = 10 PMath.* (vels!!2), lifeSpanPlanet = elapsTime}]
    | otherwise = []
    where
        elapsTime = elapsedTime gstate
        timeSpan = round elapsTime `mod` 20 == 0 && round elapsTime > 1
stepPlanetsState (x:xs) time gstate vels
    | checkDeletePlanet x elapsTime = stepPlanetsState xs time gstate vels
    | otherwise = calculateNextPositionPlanet x time : stepPlanetsState xs time gstate vels
    where
        elapsTime = elapsedTime gstate

checkDeletePlanet :: Planet -> Float -> Bool
checkDeletePlanet planet time
    | time - lifeSpanPlanet planet >= 20 && (x < -500 || x > 500 || y > 350 || y < -350) = True
    | otherwise = False
    where
        (x,y) = positionPlanet planet

calculateNextPositionPlanet :: Planet -> Float -> Planet
calculateNextPositionPlanet p time = checkWrapAround p { positionPlanet = newPos }
    where
        pos@(x, y) = positionPlanet p
        vel = velocityPlanet p
        newPos = pos PMath.+ (time PMath.* vel)

checkWrapAround :: Planet -> Planet
checkWrapAround p
    | x > 500 || x < -500 = p { positionPlanet = (-x, y) }
    | y > 350 || y < -350 = p { positionPlanet = (x, -y) }
    | otherwise = p
    where 
        (x, y) = positionPlanet p