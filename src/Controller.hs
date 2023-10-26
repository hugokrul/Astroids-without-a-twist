-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import View
import Ship
import Bullet
import Astroid

import Imports

import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath


-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = case playPauseGameOver gstate of
    Play -> 
        do
            return $ checkCollission $ checkAstroidShot $ checkGameOver $ stepGameState secs gstate
    Pause -> return gstate
    GameOver -> return gstate

checkGameOver :: GameState -> GameState
checkGameOver gstate
    | (lives $ player gstate) == 0 = gstate {playPauseGameOver=GameOver}
    | otherwise = gstate

stepGameState :: Float -> GameState -> GameState
stepGameState time gstate = gstate
                    {
                        player = checkDeleteShip $ stepPlayerState (player gstate) time,
                        bullets = stepBulletsState (bullets gstate) time,
                        astroids = stepAstroidsState (astroids gstate) time,
                        elapsedTime = elapsedTime gstate + time 
                    }
    

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey KeyUp) Down _ _) gstate = if playPauseGameOver gstate == Pause then gstate else gstate { player = moveForward (player gstate) (elapsedTime gstate)}
inputKey (EventKey (SpecialKey KeyRight) Down _ _) gstate = if playPauseGameOver gstate == Pause then gstate else gstate { player =  (player gstate) { velocityPlayer = rotateShip (player gstate) 10} }
inputKey (EventKey (SpecialKey KeyLeft) Down _ _) gstate = if playPauseGameOver gstate == Pause then gstate else gstate { player =  (player gstate) { velocityPlayer = rotateShip (player gstate) (-10)}  }
inputKey (EventKey (Char p) Down _ _) gstate 
    | p == 'p' = gstate { playPauseGameOver = Pause }
    | p == 'o' = gstate { playPauseGameOver = Play }
    | otherwise = gstate
inputKey (EventKey (SpecialKey KeyEsc) Down _ _) gstate = initialState
inputKey (EventKey (SpecialKey KeySpace) Down _ _) gstate = if playPauseGameOver gstate == Pause then gstate else fireBullet gstate
inputKey _ gstate = gstate -- Otherwise keep the same

fireBullet :: GameState -> GameState
fireBullet gstate = gstate { bullets = bullet : bullets gstate}
    where
        bullet = Bullet position (20 PMath.* vel)
        position = positionPlayer $ player gstate
        vel = velocityPlayer $ player gstate


