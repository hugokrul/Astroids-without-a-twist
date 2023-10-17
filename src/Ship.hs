module Ship where

import Graphics.Gloss

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