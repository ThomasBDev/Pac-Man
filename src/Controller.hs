-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Enemies
import Player
import Level

import Data.List

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
update :: Float -> GameState -> IO GameState
update secs gstate
  | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
  = -- We show a new random number
    do randomNumber <- randomIO
       let newNumber = abs randomNumber `mod` 10
       return $ GameState (ShowANumber newNumber) 0
  | otherwise
  = -- Just update the elapsed time
    return $ gstate { elapsedTime = elapsedTime gstate + secs }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) gstate
  = -- If the user presses a character key, show that one
    gstate { infoToShow = ShowAChar c }
inputKey _ gstate = gstate -- Otherwise keep the same




data Creature = Player Bool Position
              | Ghost Bool Bool Position
           
data Position = Position Float Float

data Direction = North
               | East
               | South
               | West

-- Level = [Row], dus Position = y-coordinaat x-coordinaat
pacManPosition = Position 8 7

selectPacMan :: Level -> Field -> Maybe Int
selectPacMan level field = elemIndex field singleList
                         where singleList = concat level