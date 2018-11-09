-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Enemies
import Player
import Level

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
update :: Float -> GameState -> IO GameState
update secs gstate
  | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
  = -- We show a new random number
    -- Dit update het level als een bepaalde tijd voorbij is gegaan.
    -- De Ghosts bewegen altijd, dus hun posities moeten hier worden aangepast?
       let updatedGhost = updatedLevelGhost (currentLevel gstate)
       in return $ GameState Playing updatedGhost 0
  | otherwise
  = -- Just update the elapsed time
    return $ gstate { elapsedTime = elapsedTime gstate + secs }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState

--Is dit waar de pacman animatie moet worden toegepast? Met een do methode?
-- Poging om Pac-Man te bewegen met de pijltjestoetsen.
inputKey (EventKey (SpecialKey KeyUp) Down _ _)    gstate = gstate { currentLevel = updatedLevel (currentLevel gstate) North }
inputKey (EventKey (SpecialKey KeyRight) Down _ _) gstate = gstate { currentLevel = updatedLevel (currentLevel gstate) East }
inputKey (EventKey (SpecialKey KeyDown) Down _ _)  gstate = gstate { currentLevel = updatedLevel (currentLevel gstate) South }
inputKey (EventKey (SpecialKey KeyLeft) Down _ _)  gstate = gstate { currentLevel = updatedLevel (currentLevel gstate) West }
    
inputKey _ gstate = gstate -- Otherwise keep the same




data Creature = Player Bool Position
              | Ghost Bool Bool Position
           


replaceAtN :: Int -> a -> [a] -> [a]
replaceAtN _ _ []     = []
replaceAtN n y (x:xs) | n == 0    = y : xs
                      | otherwise = x : replaceAtN (n-1) y xs