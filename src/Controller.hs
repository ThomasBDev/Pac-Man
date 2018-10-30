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
               deriving Eq

-- Level = [Row], dus Position = y-coordinaat x-coordinaat
pacManPosition = Position 8 7

selectPacMan :: Level -> Field -> Maybe Int
selectPacMan level field = elemIndex field singleList
                         where singleList = concat level
                        
-- Update de level array met de nieuwe positie van pac man
updatepacMan :: Level -> Maybe Int -> Direction -> Level
updatepacMan lvl Nothing _ = lvl
updatepacMan lvl (Just pos) d | checkWall lvl pos d = singleToLevel (movePac singleList pos d)
                              | otherwise          = lvl
            where
            singleList = concat lvl 
            addCorridor xs y = replaceAtN y C xs 
            singleToLevel xs = splitEvery 13 xs 
            movePac xs y d | d == North = replaceAtN (y - 13) S xs
                           | d == East  = replaceAtN (y + 1) S xs
                           | d == South = replaceAtN (y + 13) S xs
                           | d == West  = replaceAtN (y - 1) S xs

-- 13 is hier hardcoded de breedte van de pac maze want die kon ik niet vinden
checkWall :: Level -> Int -> Direction -> Bool

checkWall lvl y d | d == North = (singleList !! (y - 13)) ==  W
                  | d == East  = (singleList !! (y + 1))  ==  W
                  | d == South = (singleList !! (y + 13)) ==  W
                  | d == West  = (singleList !! (y - 1))  ==  W
                  where 
                  singleList = concat lvl
checkWall _ _ _   = False
                  
             
             
replaceAtN :: Int -> a -> [a] -> [a]
replaceAtN _ _ [] = []
replaceAtN n y (x:xs) | n == 0    = y:xs
                      | otherwise = x:replaceAtN (n-1) y xs          
             
splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = ys : splitEvery n zs 
          where (ys,zs) = splitAt n xs             
             
             
             
             
             
             
             
                         
                         
                         