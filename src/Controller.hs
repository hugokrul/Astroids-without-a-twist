-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Astroid
import Bullet
import qualified Data.Set as Set
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath
import Imports
import Model
import Planet
import Ship
import View

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = case playPauseGameOver gstate of
  Play ->
    do
      rVel1a <- randomRIO (-10.0, 10.0) :: IO Float
      rVel1b <- randomRIO (-10.0, 10.0) :: IO Float
      rVel2a <- randomRIO (-10.0, 10.0) :: IO Float
      rVel2b <- randomRIO (-10.0, 10.0) :: IO Float
      rVel3a <- randomRIO (-10.0, 10.0) :: IO Float
      rVel3b <- randomRIO (-10.0, 10.0) :: IO Float
      rVel3a <- randomRIO (-10.0, 10.0) :: IO Float
      rVel3b <- randomRIO (-10.0, 10.0) :: IO Float
      rVel4a <- randomRIO (-10.0, 10.0) :: IO Float
      rVel4b <- randomRIO (-10.0, 10.0) :: IO Float
      -- print $ elapsedTime gstate
      return $ checkCollission $ checkAstroidShot $ checkGameOver $ stepGameState secs gstate [(rVel1a, rVel1b), (rVel2a, rVel2b), (rVel3a, rVel3b), (rVel4a, rVel4b)]
  Pause -> return gstate
  GameOver -> return gstate
  Start -> return gstate

checkGameOver :: GameState -> GameState
checkGameOver gstate
  | (lives $ player gstate) == 0 = gstate {playPauseGameOver = GameOver}
  | otherwise = gstate

stepGameState :: Float -> GameState -> [Velocity] -> GameState
stepGameState time gstate randomVels =
  gstate
    { player = checkDeleteShip $ updatePosition gstate $ stepPlayerState (player gstate) time gstate,
      bullets = stepBulletsState (bullets gstate) time,
      astroids = stepAstroidsState (astroids gstate) time gstate randomVels,
      planets = stepPlanetsState (planets gstate) time $ elapsedTime gstate,
      elapsedTime = elapsedTime gstate + time
    }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = do
  print e
  return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (MouseButton LeftButton) Down _ (x, y)) gstate
  | x > -77 && x < 77 && y > -37 && y < 37 = gstate {playPauseGameOver = Play}
  | otherwise = gstate
inputKey (EventKey (SpecialKey KeySpace) Down _ _) gstate = if playPauseGameOver gstate == Pause then gstate else gstate {bullets = fireBullet gstate, keySet = Set.insert KeySpace (keySet gstate)}
inputKey (EventKey (SpecialKey KeySpace) Up _ _) gstate = if playPauseGameOver gstate == Pause then gstate else gstate {keySet = Set.delete KeySpace (keySet gstate)}
inputKey (EventKey (Char 'a') Down _ _) gstate = if playPauseGameOver gstate == Pause then gstate else gstate {player = (player gstate) {velocityPlayer = rotateShip (player gstate) (-1)}, keySet = Set.insert KeyLeft (keySet gstate)}
inputKey (EventKey (Char 'a') Up _ _) gstate = if playPauseGameOver gstate == Pause then gstate else gstate {keySet = Set.delete KeyLeft (keySet gstate)}
inputKey (EventKey (Char 'd') Down _ _) gstate = if playPauseGameOver gstate == Pause then gstate else gstate {player = (player gstate) {velocityPlayer = rotateShip (player gstate) 1}, keySet = Set.insert KeyRight (keySet gstate)}
inputKey (EventKey (Char 'd') Up _ _) gstate = if playPauseGameOver gstate == Pause then gstate else gstate {keySet = Set.delete KeyRight (keySet gstate)}
inputKey (EventKey (Char 'w') Down _ _) gstate = if playPauseGameOver gstate == Pause then gstate else gstate {player = moveForward (player gstate) (elapsedTime gstate), keySet = Set.insert KeyUp (keySet gstate)}
inputKey (EventKey (Char 'w') Up _ _) gstate = if playPauseGameOver gstate == Pause then gstate else gstate {keySet = Set.delete KeyUp (keySet gstate)}
inputKey (EventKey (Char p) Down _ _) gstate
  | p == 'p' = gstate {playPauseGameOver = Pause}
  | p == 'o' = gstate {playPauseGameOver = Play}
  | otherwise = gstate
inputKey (EventKey (SpecialKey KeyEsc) Down _ _) gstate = initialState
inputKey _ gstate = gstate -- Otherwise keep the same