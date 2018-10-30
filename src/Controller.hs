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
               deriving Eq

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
   

   
testUpdate :: Direction -> IO()
testUpdate dir = printLevel updatedLevel
               where pacManIndex  = selectPacMan testLevel
                     updatedLevel = updatepacMan testLevel pacManIndex dir
                                         
-- Update de level array met de nieuwe positie van pac man
updatepacMan :: Level -> Maybe Int -> Direction -> Level
updatepacMan lvl Nothing _    = [[]]
updatepacMan lvl (Just pos) d | checkWall lvl pos d = lvl
                              | otherwise           = singleToLevel (movePac singleList pos d)
                              where singleList       = concat lvl 
                                    addCorridor xs y = replaceAtN y C xs 
                                    singleToLevel xs = splitEvery 13 xs 
                                    movePac xs y d   | d == North = replaceAtN (y - 13) S xs
                                                     | d == East  = replaceAtN (y + 1) S xs
                                                     | d == South = replaceAtN (y + 13) S xs
                                                     | d == West  = replaceAtN (y - 1) S xs

                           
                           
testCheck :: Direction -> Bool
testCheck dir = checkWall testLevel pacManIndex dir
              where pacManIndex = removeMaybe (selectPacMan testLevel)
              
removeMaybe :: Maybe a -> a
removeMaybe (Just x) = x
                           
-- 13 is hier hardcoded de breedte van de pac maze want die kon ik niet vinden
checkWall :: Level -> Int -> Direction -> Bool
checkWall lvl y d | d == North = (singleList !! (y - 13)) ==  W
                  | d == East  = (singleList !! (y + 1))  ==  W
                  | d == South = (singleList !! (y + 13)) ==  W
                  | d == West  = (singleList !! (y - 1))  ==  W
                  where singleList = concat lvl
checkWall _ _ _   = False
                  
            

testReplaceRow :: Row
testReplaceRow = [ W, C, C, C, W,
                   W, C, S, C, W,
                   W, C, C, C, W ]            

testReplace :: Direction -> Row
testReplace North = replaceAtN2 2 7 S testReplaceRow
                                -- newIndex (oldIndex = newIndex - oldIndex, omdat newIndex in de functie aftelt en niet optelt.)
testReplace East  = replaceAtN1 8 (8 - 7) S testReplaceRow
testReplace South = replaceAtN1 12 (12 - 7) S testReplaceRow
testReplace West  = replaceAtN2 7 6 S testReplaceRow
            
replaceAtN :: Int -> a -> [a] -> [a]
replaceAtN _ _ []     = []
replaceAtN n y (x:xs) | n == 0    = y : xs
                      | otherwise = x : replaceAtN (n-1) y xs   

-- Deze functie werkt alleen met East en South omdat de index van de oude positie kleiner is dan de nieuwe positie.
-- De functie stopt pas als de nieuwe index "op" is, dus de oude positie komt daarvoor nog langs.
replaceAtN1 :: Int -> Int -> Field -> Row -> Row
replaceAtN1 _ _ _ []                          = []
replaceAtN1 newIndex oldIndex newField (x:xs) | newIndex == 0        = newField : xs
                                              | newIndex == oldIndex = T : replaceAtN1 (newIndex - 1) oldIndex newField xs
                                              | otherwise            = x : replaceAtN1 (newIndex - 1) oldIndex newField xs
                                              
-- Je moet
replaceAtN2 :: Int -> Int -> Field -> Row -> Row
replaceAtN2 _ _ _ []                          = []
replaceAtN2 newIndex oldIndex newField (x:xs) | newIndex == 0        = newField : xs
                                              | newIndex == oldIndex = T : replaceAtN2 (newIndex - 1) oldIndex newField xs
                                              | otherwise            = x : replaceAtN2 (newIndex - 1) oldIndex newField xs
                                              
splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = ys : splitEvery n zs 
                where (ys,zs) = splitAt n xs