-- | This module contains the data types
--   which represent the state of the game
module Model where

import Level

data TypeOfState = Title
                 | Playing
                 | ShowAChar   Char

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 2

data GameState = GameState {
                   typeOfState  :: TypeOfState
                 , currentLevel :: Level
                 , elapsedTime  :: Float
                 }
                 
-- data GameState = Title
               -- | Playing
               -- | Paused
               -- | GameOver

initialState :: GameState
initialState = GameState Title testLevel 0