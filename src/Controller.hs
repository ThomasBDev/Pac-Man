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
           
data Position = Position Int Int

instance Show Position where
    show (Position x y) = show x ++ " " ++ show y

data Direction = North
               | East
               | South
               | West

-- Level = [Row], dus Position = y-coordinaat x-coordinaat
pacManPosition = Position 8 7

selectPacMan :: Level -> Maybe Int
selectPacMan level = elemIndex S singleList
                   where singleList = concat level
                         
currentPacManPosition :: Maybe Int -> Position
currentPacManPosition Nothing      = Position 0 0
currentPacManPosition (Just index) = Position x y
                                   where x = index `mod` levelWidth
                                         y = index `div` levelWidth
                                         
{-                         
-- Update de level array met de nieuwe positie van pac man
updatepacMan :: Level -> Int -> Direction -> Level
updatepacMan _ Nothing _ = Nothing
updatepacMan xs y d | checkWall (xs y d) = movePac xs y d
                    | otherwise          = xs
            where
            singleList = concat xs -- Geen idee hoe je de huidige waarde aanpast. recursief?
            movePac xs y d | d == North = Nothing
                           | d == East  = Nothing 
                           | d == South = Nothing
                           | d == West  = Nothing

-- 13 is hier hardcoded de breedte van de pac maze want die kon ik niet vinden
checkWall :: Level -> Int -> Direction -> Bool
checkWall _ Nothing _ = Nothing
checkWall xs y d | d == North = (singleList !! (y - 13)) == not W
                 | d == East  = (singleList !! (y + 1))  == not W
                 | d == South = (singleList !! (y + 13)) == not W
                 | d == West  = (singleList !! (y - 1))  == not W
            where 
            singleList = concat xs
             -}