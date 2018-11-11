-- | This module contains the data types
--   which represent the state of the game
module Model where

import Level

data TypeOfState = Title
                 | Playing
                 | Paused
                 | GameOver
                 deriving Eq

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 1

data GameState = GameState {
                   typeOfState       :: TypeOfState
                 , initialLevels     :: [Level]
                 , currentLevel      :: Level
                 , elapsedTime       :: Float
                 , randomLevelIndex  :: Int
                 , currentScore      :: Int
                 , currentHighScore  :: Int
                 }

initialState :: GameState
initialState = GameState Title [] [] 0 0 0 0