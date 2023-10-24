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

checkDeleteShip :: GameState -> GameState
checkDeleteShip gstate
    | x < -380 || x > 380 || y > 220 || y < -220 = gstate {playPauseGameOver=GameOver}
    | otherwise = gstate
    where 
        x = fst $ positionPlayer $ player gstate
        y = snd $ positionPlayer $ player gstate