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
                   typeOfState  :: TypeOfState
                 , currentLevel :: Level
                 , elapsedTime  :: Float
                 , currentScore :: Int
                 }

initialState :: GameState
initialState = GameState Title testLevel 0 0