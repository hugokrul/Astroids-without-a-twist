module Ship where

import Graphics.Gloss
import Model


ship :: Picture
ship = pictures 
    [
        leftLine,
        rightLine,
        middleLine
    ]
    where
        leftLine    = translate (-9)    0   $ rotate 20     $ color white $ rectangleSolid 1 50
        rightLine   = translate   9     0   $ rotate (-20)  $ color white $ rectangleSolid 1 50
        middleLine  = translate   0  (-10)  $ rotate 90     $ color white $ rectangleSolid 1 25


givePIS :: World -> PointInSpace
givePIS (Play (Player p _ _ _) _) = p

giveVelocity :: World -> Velocity
giveVelocity (Play (Player _ v _ _) _) = v

giveAcceleration :: World -> Acceleration
giveAcceleration (Play (Player _ _ a _) _) = a

giveDir :: World -> Direction
giveDir (Play (Player _ _ _ d) _) = d