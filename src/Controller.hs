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
       return $ GameState (ShowANumber newNumber) testLevel 0
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

-- Poging om Pac-Man te bewegen met de pijltjestoetsen.
inputKey (EventKey (SpecialKey KeyUp) _ _ _)    gstate = gstate { currentLevel = updatedLevel North }
inputKey (EventKey (SpecialKey KeyRight) _ _ _) gstate = gstate { currentLevel = updatedLevel East }
inputKey (EventKey (SpecialKey KeyDown) _ _ _)  gstate = gstate { currentLevel = updatedLevel South }
inputKey (EventKey (SpecialKey KeyLeft) _ _ _)  gstate = gstate { currentLevel = updatedLevel West }
    
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
                     updatedLevel = updatePacMan testLevel pacManIndex dir
 

 
updatedLevel1, updatedLevel2, updatedLevel3, updatedLevel4 :: Level
updatedLevel1 = updatedLevel North
updatedLevel2 = updatedLevel East
updatedLevel3 = updatedLevel South
updatedLevel4 = updatedLevel West

updatedLevel :: Direction -> Level
updatedLevel dir = updatePacMan testLevel pacManIndex dir
                 where pacManIndex = selectPacMan testLevel
 

 
-- Update de level array met de nieuwe positie van pac man
updatePacMan :: Level -> Maybe Int -> Direction -> Level
updatePacMan lvl Nothing _    = [[]]
updatePacMan lvl (Just pos) d | checkWall lvl pos d = lvl
                              | otherwise           = singleToLevel (movePac singleList pos d)
                              where singleList       = concat lvl
                                    singleToLevel xs = splitEvery levelWidth xs
                                    movePac xs y d   | d == North = replaceAtN2 (y - levelWidth) y S xs
                                                     | d == East  = replaceAtN1 (y + 1)          y S xs
                                                     | d == South = replaceAtN1 (y + levelWidth) y S xs
                                                     | d == West  = replaceAtN2 (y - 1)          y S xs
                                    {-
                                    singleToLevel xs = splitEvery 5 xs
                                    movePac xs y d   | d == North = replaceAtN2 (y - 5) y S xs
                                                     | d == East  = replaceAtN1 (y + 1) y S xs
                                                     | d == South = replaceAtN1 (y + 5) y S xs
                                                     | d == West  = replaceAtN2 (y - 1) y S xs
                                                     -}



testCheck :: Direction -> Bool
testCheck dir = checkWall testLevel pacManIndex dir
              where pacManIndex = removeMaybe (selectPacMan testLevel)
              
removeMaybe :: Maybe a -> a
removeMaybe (Just x) = x
                           
-- 13 is hier hardcoded de breedte van de pac maze want die kon ik niet vinden
checkWall :: Level -> Int -> Direction -> Bool
checkWall lvl y d | d == North = (singleList !! (y - levelWidth)) ==  W
                  | d == East  = (singleList !! (y + 1))          ==  W
                  | d == South = (singleList !! (y + levelWidth)) ==  W
                  | d == West  = (singleList !! (y - 1))          ==  W
{-
checkWall lvl y d | d == North = (singleList !! (y - 5)) ==  W
                  | d == East  = (singleList !! (y + 1)) ==  W
                  | d == South = (singleList !! (y + 5)) ==  W
                  | d == West  = (singleList !! (y - 1)) ==  W
                  -}
                  where singleList = concat lvl
checkWall _ _ _   = False
                  
            

{-
testReplaceLevel :: Level
testReplaceLevel = [ [W, C, C, C, W],
                     [W, C, S, C, W],
                     [W, C, C, C, W] ]
            
testReplaceRow :: Row
testReplaceRow = concat testReplaceLevel
                   
testReplaceLevelDraw :: Direction -> IO()
testReplaceLevelDraw dir = printLevel replacedLevel
                         where replacedLevel = updatePacMan testReplaceLevel pacManIndex dir
                               pacManIndex   = selectPacMan testReplaceLevel

testReplace :: Direction -> Row
testReplace North = replaceAtN2 2  7 S testReplaceRow
testReplace East  = replaceAtN1 8  7 S testReplaceRow
testReplace South = replaceAtN1 12 7 S testReplaceRow
testReplace West  = replaceAtN2 6  7 S testReplaceRow
-}



replaceAtN :: Int -> a -> [a] -> [a]
replaceAtN _ _ []     = []
replaceAtN n y (x:xs) | n == 0    = y : xs
                      | otherwise = x : replaceAtN (n-1) y xs   

-- Deze functie werkt alleen met East en South omdat de index van de oude positie kleiner is dan de nieuwe positie.
-- De functie stopt pas als de nieuwe index "op" is, dus de oude positie komt daarvoor nog langs.
replaceAtN1 :: Int -> Int -> Field -> Row -> Row
replaceAtN1 _ _ _ []                          = []
replaceAtN1 newIndex oldIndex newField (x:xs) | newIndex == 0 = newField : xs
                                              | oldIndex == 0 = C : replaceAtN1 (newIndex - 1) (oldIndex - 1) newField xs
                                              | otherwise     = x : replaceAtN1 (newIndex - 1) (oldIndex - 1) newField xs
                                              
-- Je moet
replaceAtN2 :: Int -> Int -> Field -> Row -> Row
replaceAtN2 _ _ _ []                          = []
replaceAtN2 newIndex oldIndex newField (x:xs) | newIndex == 0 = newField : replaceAtN2 (newIndex - 1) (oldIndex - 1) newField xs
                                              | oldIndex == 0 = C : xs
                                              | otherwise     = x : replaceAtN2 (newIndex - 1) (oldIndex - 1) newField xs
                                              
splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = ys : splitEvery n zs 
                where (ys,zs) = splitAt n xs