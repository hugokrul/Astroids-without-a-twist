-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import View
import Ship
import Bullet
import Astroid
import Planet

import Imports
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath
import qualified Data.Set as Set

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = case playPauseGameOver gstate of
    Play ->
        do
            print $ elapsedTime gstate
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
                        player = checkDeleteShip $ updatePosition gstate $ stepPlayerState (player gstate) time gstate,
                        bullets = stepBulletsState (bullets gstate) time,
                        astroids = stepAstroidsState (astroids gstate) time,
                        planets = stepPlanetsState (planets gstate) time $ elapsedTime gstate,
                        elapsedTime = elapsedTime gstate + time
                    }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)


inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey KeySpace) Down _ _) gstate = if playPauseGameOver gstate == Pause then gstate else gstate { bullets = fireBullet gstate, keySet = Set.insert KeySpace (keySet gstate) }
inputKey (EventKey (SpecialKey KeySpace) Up _ _) gstate = if playPauseGameOver gstate == Pause then gstate else gstate { keySet = Set.delete KeySpace (keySet gstate) }

inputKey (EventKey (Char 'a') Down _ _) gstate = if playPauseGameOver gstate == Pause then gstate else gstate { player =  (player gstate) { velocityPlayer = rotateShip (player gstate) (-1)}, keySet = Set.insert KeyLeft (keySet gstate)  }
inputKey (EventKey (Char 'a') Up _ _) gstate = if playPauseGameOver gstate == Pause then gstate else gstate { keySet = Set.delete KeyLeft (keySet gstate)}

inputKey (EventKey (Char 'd') Down _ _) gstate = if playPauseGameOver gstate == Pause then gstate else gstate { player =  (player gstate) { velocityPlayer = rotateShip (player gstate) 1}, keySet = Set.insert KeyRight (keySet gstate) }
inputKey (EventKey (Char 'd') Up _ _) gstate = if playPauseGameOver gstate == Pause then gstate else gstate { keySet = Set.delete KeyRight (keySet gstate)}

inputKey (EventKey (Char 'w') Down _ _) gstate = if playPauseGameOver gstate == Pause then gstate else gstate { player = moveForward (player gstate) (elapsedTime gstate), keySet = Set.insert KeyUp (keySet gstate)}
inputKey (EventKey (Char 'w') Up _ _) gstate = if playPauseGameOver gstate == Pause then gstate else gstate { keySet = Set.delete KeyUp (keySet gstate)}

inputKey (EventKey (Char p) Down _ _) gstate
    | p == 'p' = gstate { playPauseGameOver = Pause }
    | p == 'o' = gstate { playPauseGameOver = Play }
    | otherwise = gstate
inputKey (EventKey (SpecialKey KeyEsc) Down _ _) gstate = initialState
inputKey _ gstate = gstate -- Otherwise keep the same