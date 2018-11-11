module Level where

import Data.List

fieldWidth :: Float
fieldWidth = 50

windowWidth, windowHeight :: Float
windowWidth  = 650
windowHeight = 650

levelWidth, levelHeight :: Int
levelWidth  = 13
levelHeight = 11

type Level = [Row]
type Row   = [Field]

data Direction = North
               | East
               | South
               | West
               deriving Eq
              
allDirections :: [Direction]
allDirections = [North, East, South, West]

data Field = C --Corridor
           | D --Dot
           | W --Wall
           | T --Teleporter
           | S --Pac-Man
           | G --Ghost
           deriving Eq
 
readStringFromFile :: FilePath -> IO [String]
readStringFromFile path = do strings <- readFile ("src/Levels/" ++ path)
                             return $ lines strings

loadLevels :: [IO [String]]
loadLevels = map readStringFromFile ["Level 1.txt", "Level 2.txt", "Level 3.txt", "Level 4.txt"]
 
convertStringsToLevels :: [[String]] -> [Level]
convertStringsToLevels []   = []
convertStringsToLevels list = map (map (map convertCharToField)) list

convertCharToField :: Char -> Field
convertCharToField 'C' = C
convertCharToField 'D' = D
convertCharToField 'W' = W
convertCharToField 'T' = T
convertCharToField 'S' = S
convertCharToField 'G' = G
    
    
    
data Position = Pos Int Int

instance Show Position where
    show (Pos x y) = show x ++ " " ++ show y

currentPacManPosition :: Maybe Int -> Position
currentPacManPosition (Just index) = Pos x y
                                   where x = index `mod` levelWidth
                                         y = index `div` levelWidth                      

-- Check if a specified Field is in the specified Direction.
checkField :: Level -> Field -> Int -> Direction -> Bool
checkField level field pos dir | dir == North = singleList !! (pos - levelWidth) ==  field
                               | dir == East  = singleList !! (pos + 1)          ==  field
                               | dir == South = singleList !! (pos + levelWidth) ==  field
                               | dir == West  = singleList !! (pos - 1)          ==  field
                               where singleList = concat level
checkField _ _ _ _             = False

-- Give the index value of Pac-Man or the Ghost.                                         
selectCreature :: Level -> Field -> Maybe Int
selectCreature level field = elemIndex field (concat level)

-- Move Pac-Man or the Ghost through a Level.
moveCreature :: Row -> Int -> Field -> Direction -> Row
moveCreature currentLevel creatureIndex field North = replaceAtN2 (creatureIndex - levelWidth) creatureIndex field currentLevel
moveCreature currentLevel creatureIndex field East  = replaceAtN1 (creatureIndex + 1)          creatureIndex field currentLevel
moveCreature currentLevel creatureIndex field South = replaceAtN1 (creatureIndex + levelWidth) creatureIndex field currentLevel
moveCreature currentLevel creatureIndex field West  = replaceAtN2 (creatureIndex - 1)          creatureIndex field currentLevel

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = ys : splitEvery n zs 
                where (ys,zs) = splitAt n xs

-- Switch Field A with Field B, if A has a smaller index than B.
replaceAtN1 :: Int -> Int -> Field -> Row -> Row
replaceAtN1 _ _ _ []                          = []
replaceAtN1 newIndex oldIndex newField (x:xs) | newIndex == 0 = newField : xs
                                              | oldIndex == 0 = C : replaceAtN1 (newIndex - 1) (oldIndex - 1) newField xs
                                              | otherwise     = x : replaceAtN1 (newIndex - 1) (oldIndex - 1) newField xs
                                              
-- Switch Field A with Field B, if A has a greater index than B.
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