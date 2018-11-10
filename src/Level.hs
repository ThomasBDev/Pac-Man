module Level where

import Data.List

-- putStrLn neemt een String en tekent de uitkomst in de console.
printLevel :: Level -> IO ()
printLevel rows = putStrLn result
                where result = concat (map printRow rows)
                      
printRow :: Row -> String
printRow row = concat (map show row) ++ "\n"

pacMouth :: Float
pacMouth = 40

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

row13Walls, row6Walls, row5Walls, row4Walls, row9Corridors, row7Corridors, row3Corridors :: Row
row13Walls = [W, W, W, W, W, W, W, W, W, W, W, W, W]
row6Walls = [W, W, W, W, W, W]
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
              [T, C, W] ++ row3Corridors ++ [G] ++ row3Corridors ++ [W, C, T],
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
              
allDirections :: [Direction]
allDirections = [North, East, South, West]

data Field = C --Corridor
           | D --Dot
           | P --PowerDot
           | W --Wall
           | T --Teleporter
           | H --Home
           | S --Pac-Man
           | G --Ghost
           deriving Eq

instance Show Field where
    show C = " "
    show D = "."
    show P = "+"
    show W = "#"
    show T = "*"
    show H = "_"
    show S = "S"
    show G = "G"
    
data Position = Pos Int Int

instance Show Position where
    show (Pos x y) = show x ++ " " ++ show y

currentPacManPosition :: Maybe Int -> Position
currentPacManPosition (Just index) = Pos x y
                                   where x = index `mod` levelWidth
                                         y = index `div` levelWidth                      



-- 13 is hier hardcoded de breedte van de pac maze want die kon ik niet vinden
checkWall :: Level -> Int -> Direction -> Bool
checkWall lvl y d | d == North = (singleList !! (y - levelWidth)) ==  W
                  | d == East  = (singleList !! (y + 1))          ==  W
                  | d == South = (singleList !! (y + levelWidth)) ==  W
                  | d == West  = (singleList !! (y - 1))          ==  W
                  where singleList = concat lvl
checkWall _ _ _   = False

checkTeleporter :: Level -> Int -> Direction -> Bool
checkTeleporter lvl y d | d == North = (singleList !! (y - levelWidth)) == T
                        | d == East  = (singleList !! (y + 1))          == T
                        | d == South = (singleList !! (y + levelWidth)) == T
                        | d == West  = (singleList !! (y - 1))          == T
                        where singleList = concat lvl
checkTeleporter _ _ _   = False

selectCreature :: Level -> Field -> Maybe Int
selectCreature level field = elemIndex field (concat level)

-- Deze dient een animatie toegevoegd te krijgen (Dus het moet een paar frames duren en bewegen)
moveCreature :: Row -> Int -> Field -> Direction -> Row
moveCreature currentLevel creatureIndex field North = replaceAtN2 (creatureIndex - levelWidth) creatureIndex field currentLevel
moveCreature currentLevel creatureIndex field East  = replaceAtN1 (creatureIndex + 1)          creatureIndex field currentLevel
moveCreature currentLevel creatureIndex field South = replaceAtN1 (creatureIndex + levelWidth) creatureIndex field currentLevel
moveCreature currentLevel creatureIndex field West  = replaceAtN2 (creatureIndex - 1)          creatureIndex field currentLevel



splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = ys : splitEvery n zs 
                where (ys,zs) = splitAt n xs

-- Deze functie werkt alleen met East en South, omdat de index van de oude positie kleiner is dan de nieuwe positie.
-- De functie stopt pas als de nieuwe index "op" is, dus de oude positie komt daarvoor nog langs.
replaceAtN1 :: Int -> Int -> Field -> Row -> Row
replaceAtN1 _ _ _ []                          = []
replaceAtN1 newIndex oldIndex newField (x:xs) | newIndex == 0 = newField : xs
                                              | oldIndex == 0 = C : replaceAtN1 (newIndex - 1) (oldIndex - 1) newField xs
                                              | otherwise     = x : replaceAtN1 (newIndex - 1) (oldIndex - 1) newField xs
                                              
-- Deze functie werkt alleen met North en West, omdat de index van de oude positie groter is dan de nieuwe positie.
replaceAtN2 :: Int -> Int -> Field -> Row -> Row
replaceAtN2 _ _ _ []                          = []
replaceAtN2 newIndex oldIndex newField (x:xs) | newIndex == 0 = newField : replaceAtN2 (newIndex - 1) (oldIndex - 1) newField xs
                                              | oldIndex == 0 = C : xs
                                              | otherwise     = x : replaceAtN2 (newIndex - 1) (oldIndex - 1) newField xs
               
               
 
data Item = PacDot
          | Fruit
          
data PacDot = Dot
            | PowerDot
            
instance Show PacDot where
    show Dot      = "D"
    show PowerDot = "P"
            
data Fruit = Float