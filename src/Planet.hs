module Planet where

import Imports
import Model
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath

planet :: Picture
planet = color white $ thickCircle 50 100

showPlanet :: Planet -> Picture
showPlanet p = uncurry translate (positionPlanet p) planet

stepPlanetsState :: [Planet] -> Float -> Float -> [Planet]
stepPlanetsState [] _ _ = []
stepPlanetsState (x:xs) time elapsTime
    | checkDeletePlanet x elapsTime = stepPlanetsState xs time elapsTime
    | otherwise = calculateNextPositionPlanet x time : stepPlanetsState xs time elapsTime

checkDeletePlanet :: Planet -> Float -> Bool
checkDeletePlanet planet time
    | time - lifeSpanPlanet planet >= 10 && (x < -500 || x > 500 || y > 350 || y < -350) = True
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
