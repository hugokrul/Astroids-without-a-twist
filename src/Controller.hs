-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import View
import Ship
import Bullet
import Astroid

import Imports


-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = case playPauseGameOver gstate of
    Play -> do return $ gstate { elapsedTime = elapsedTime gstate + secs }
    Pause -> return gstate
    GameOver -> return gstate

stepGameState :: Float -> GameState -> GameState
stepGameState time gstate = gstate
                    {
                        player = stepPlayerState (player gstate) time
                    }
    

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey KeyUp) Down _ _) gstate = gstate { player = (player gstate) { positionPlayer = moveForward $ player gstate } }
inputKey (EventKey (SpecialKey KeyRight) Down _ _) gstate = gstate { player =  (player gstate) { velocityPlayer = rotateShip (player gstate) 10} }
inputKey (EventKey (SpecialKey KeyLeft) Down _ _) gstate = gstate { player =  (player gstate) { velocityPlayer = rotateShip (player gstate) (-10)}  }
inputKey (EventKey (SpecialKey KeyEsc) Down _ _) gstate = initialState
inputKey (EventKey (SpecialKey KeySpace) Down _ _) gstate = fireBullet gstate
inputKey _ gstate = gstate -- Otherwise keep the same

fireBullet :: GameState -> GameState
fireBullet gstate = gstate { bullets = bullet : bullets gstate}
    where
        bullet = Bullet position (200, 0) lifespan
        position = positionPlayer $ player gstate
        lifespan = elapsedTime gstate


