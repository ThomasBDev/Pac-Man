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
  | typeOfState gstate == Title = return gstate
  | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
  = -- We show a new random number
    -- Dit update het level als een bepaalde tijd voorbij is gegaan.
    -- De Ghosts bewegen altijd, dus hun posities moeten hier worden aangepast?       
       do randomIndex <- randomRIO (0, 1)
          let updatedGhost = updatedLevelGhost (currentLevel gstate) randomIndex
          return $ GameState Playing updatedGhost 0
  | otherwise
  = -- Just update the elapsed time
    return $ gstate { elapsedTime = elapsedTime gstate + secs }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey KeySpace) Down _ _) gstate | typeOfState gstate == Title = gstate { typeOfState = Playing }
                                                          | otherwise                   = gstate 
inputKey (EventKey (SpecialKey keyType) Down _ _) gstate | typeOfState gstate == Playing = newGameState gstate keyType
                                                         | otherwise                     = gstate   
inputKey _ gstate = gstate -- Otherwise keep the same

newGameState :: GameState -> SpecialKey -> GameState
newGameState gstate KeyUp    = gstate { currentLevel = updatedLevel (currentLevel gstate) North }
newGameState gstate KeyRight = gstate { currentLevel = updatedLevel (currentLevel gstate) East }
newGameState gstate KeyDown  = gstate { currentLevel = updatedLevel (currentLevel gstate) South }
newGameState gstate KeyLeft  = gstate { currentLevel = updatedLevel (currentLevel gstate) West }



data Creature = Player Bool Position
              | Ghost Bool Bool Position
           


replaceAtN :: Int -> a -> [a] -> [a]
replaceAtN _ _ []     = []
replaceAtN n y (x:xs) | n == 0    = y : xs
                      | otherwise = x : replaceAtN (n-1) y xs