module Level where

import Data.List

-- putStrLn neemt een String en tekent de uitkomst in de console.
printLevel :: Level -> IO ()
printLevel rows = putStrLn result
                where result = concat (map printRow rows)
                      
printRow :: Row -> String
printRow row = concat (map show row) ++ "\n"



fieldWidth :: Float
fieldWidth = 50

windowWidth, windowHeight :: Float
windowWidth  = 650
windowHeight = 650

levelWidth, levelHeight :: Int
levelWidth  = length (testLevel !! 0)
levelHeight = length testLevel

type Level = [Row]
type Row   = [Field]

row13Walls, row5Walls, row4Walls, row9Corridors, row7Corridors, row3Corridors :: Row
row13Walls = [W, W, W, W, W, W, W, W, W, W, W, W, W]
row5Walls = [W, W, W, W, W]
row4Walls = [W, W, W, W]
row9Corridors = [C, C, C, C, C, C, C, C, C]
row7Corridors = [C, C, C, C, C, C, C]
row3Corridors = [C, C, C]
row9Dots = [D, D, D, D, D, D, D, D, D]
row7Dots = [D, D, D, D, D, D, D]
row3Dots = [D, D, D]
           
testLevel :: Level
testLevel = [ row13Walls,
              [W, P] ++ row9Dots ++ [P, W],
              [W, C] ++ row4Walls ++ [C] ++ row4Walls ++ [C, W],
              [W, C, W] ++ row7Corridors ++ [W, C, W],
              [W] ++ row3Dots ++ [W, W, C, W, W] ++ row3Dots ++ [W],
              [T, C, W, C, W, H, H, H, W, C, W, C, T],
              [W] ++ row3Dots++ row5Walls ++ row3Dots ++ [W],
              [W, C, W] ++ row3Corridors ++ [S] ++ row3Corridors ++ [W, C, W],
              [W, C] ++ row4Walls ++ [C] ++ row4Walls ++ [C, W],
              [W, P] ++ row9Dots ++ [P, W],
              row13Walls ]

data Direction = North
               | East
               | South
               | West
               deriving Eq

data Field = C --Corridor
           | D --Dot
           | P --PowerDot
           | W --Wall
           | T --Teleporter
           | H --Home
           | S --Pac-Man spawn
           deriving Eq

instance Show Field where
    show C = " "
    show D = "."
    show P = "+"
    show W = "#"
    show T = "*"
    show H = "_"
    show S = "S"



updatedLevel :: Level -> Direction -> Level
updatedLevel level dir = updatePacMan level pacManIndex dir
                       where pacManIndex = selectPacMan level
                       
selectPacMan :: Level -> Maybe Int
selectPacMan level = elemIndex S singleList
                   where singleList = concat level
                       
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

-- 13 is hier hardcoded de breedte van de pac maze want die kon ik niet vinden
checkWall :: Level -> Int -> Direction -> Bool
checkWall lvl y d | d == North = (singleList !! (y - levelWidth)) ==  W
                  | d == East  = (singleList !! (y + 1))          ==  W
                  | d == South = (singleList !! (y + levelWidth)) ==  W
                  | d == West  = (singleList !! (y - 1))          ==  W
                  where singleList = concat lvl
checkWall _ _ _   = False

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = ys : splitEvery n zs 
                where (ys,zs) = splitAt n xs

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

                                              
                                              
teleportPacMan :: Int -> Int
teleportPacMan xTeleporter | xTeleporter == 0                = levelWidth - 2
                           | xTeleporter == (levelWidth - 1) = 1
                           | otherwise                       = -1



data Item = PacDot
          | Fruit
          
data PacDot = Dot
            | PowerDot
            
instance Show PacDot where
    show Dot      = "D"
    show PowerDot = "P"
            
data Fruit = Float