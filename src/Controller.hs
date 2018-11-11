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

import Data.Char

-- | Handle one iteration of the game
update :: Float -> GameState -> IO GameState
update secs gstate
  | typeOfState gstate == Title    = do highscore <- loadScore                                        
                                        levelStrings <- sequence loadLevels
                                        randomLevel <- randomRIO (0, ((length levelStrings) - 1))
                                        return $ gstate { currentHighScore = highscore, allLevels = convertStringsToLevels levelStrings, randomLevelIndex = randomLevel }
  | typeOfState gstate == Paused   = return gstate
  | typeOfState gstate == GameOver = return gstate
  
  -- We zitten in de PlayingState en 1 cyclus is voorbij.
  | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
  = -- We show a new random number
    -- Dit update het level als een bepaalde tijd voorbij is gegaan.
    -- De Ghosts bewegen altijd, dus hun posities moeten hier worden aangepast?       
       do randomIndex <- randomRIO (0, 1)
          let updatedGhost = updatedLevelGhost (currentLevel gstate) randomIndex
          -- Pac-Man of Ghost is weg --> Ghost en Pac-man zijn gebotst.
          if ((selectCreature (currentLevel gstate) S) == Nothing) || ((selectCreature (currentLevel gstate) G) == Nothing)
          then return $ gstate { typeOfState = GameOver }
          else return $ gstate { currentLevel = updatedGhost, elapsedTime = 0 }
  -- We zitten in de PlayingState, maar de cyclus is nog bezig.
  | otherwise
  = -- Just update the elapsed time
    -- Er zijn geen dots meer --> Pac-Man heeft gewonnen, ga naar het volgende level.
    if (allDots (currentLevel gstate) == Nothing)
    then return $ gstate { currentLevel = initialLevel gstate, currentHighScore = currentScore gstate }
    else return $ gstate { elapsedTime = elapsedTime gstate + secs }
    
allDots :: Level -> Maybe Int
allDots []    = Nothing
allDots level = selectCreature level D

    
    
-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState                                                         
inputKey (EventKey key Down _ _) gstate | typeOfState gstate == Title                                  = selectLevel gstate key
                                        | typeOfState gstate == Playing                                = newGameState gstate key
                                        | typeOfState gstate == Paused && key == Char 'p'              = gstate { typeOfState = Playing }
                                        | typeOfState gstate == GameOver && key == SpecialKey KeySpace = initialState
                                        | otherwise                                                    = gstate
inputKey _ gstate = gstate -- Otherwise keep the same

selectLevel :: GameState -> Key -> GameState
selectLevel gstate (Char 'r')                                                       = gstate { typeOfState = Playing, initialLevel = chosenLevel, currentLevel = chosenLevel }
                                                                                    where chosenLevel = allLevels gstate !! randomLevelIndex gstate
                              -- There are no levels to choose from, stay in the Title state.
selectLevel gstate (Char c) | (allLevels gstate == [])                              = gstate
                            | let index = ord c - 49
                              -- The (number) key pressed has a lower or higher value than there are levels.
                              -- You can't select level N + 1 out of N levels.
                              in index < 0 || index > length (allLevels gstate) - 1 = gstate                               
                            | otherwise                                             = gstate { typeOfState = Playing, initialLevel = chosenLevel, currentLevel = chosenLevel }
                                                                                    where chosenLevel = allLevels gstate !! (ord c - 49)
selectLevel gstate _                                                                = gstate
                            
newGameState :: GameState -> Key -> GameState
newGameState gstate (SpecialKey KeyUp)    = gstate { currentLevel = updatedLevel (currentLevel gstate) North, currentScore = updatedScore (checkDot (currentLevel gstate) (pacManIndex (currentLevel gstate)) North) (currentScore gstate) }
newGameState gstate (SpecialKey KeyRight) = gstate { currentLevel = updatedLevel (currentLevel gstate) East,  currentScore = updatedScore (checkDot (currentLevel gstate) (pacManIndex (currentLevel gstate)) East) (currentScore gstate) }
newGameState gstate (SpecialKey KeyDown)  = gstate { currentLevel = updatedLevel (currentLevel gstate) South, currentScore = updatedScore (checkDot (currentLevel gstate) (pacManIndex (currentLevel gstate)) South) (currentScore gstate) }
newGameState gstate (SpecialKey KeyLeft)  = gstate { currentLevel = updatedLevel (currentLevel gstate) West,  currentScore = updatedScore (checkDot (currentLevel gstate) (pacManIndex (currentLevel gstate)) West) (currentScore gstate) }
newGameState gstate (Char 'p')            = gstate { typeOfState = Paused }
newGameState gstate (Char 'q')            = initialState
newGameState gstate _                     = gstate

pacManIndex :: Level -> Int
pacManIndex currentLevel = removeMaybe (selectCreature currentLevel S)

removeMaybe :: Maybe Int -> Int
removeMaybe Nothing  = -1
removeMaybe (Just x) = x



data Creature = Player Bool Position
              | Ghost Bool Bool Position
           


replaceAtN :: Int -> a -> [a] -> [a]
replaceAtN _ _ []     = []
replaceAtN n y (x:xs) | n == 0    = y : xs
                      | otherwise = x : replaceAtN (n-1) y xs