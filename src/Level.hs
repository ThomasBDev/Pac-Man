module Level where

-- putStrLn neemt een String en tekent de uitkomst in de console.
printLevel :: Level -> IO ()
printLevel rows = putStrLn result
                where result = concat (map printRow rows)
                      
printRow :: Row -> String
printRow row = concat (map show row) ++ "\n"

eatPacDot :: Field -> Field
eatPacDot D = C
eatPacDot P = C
eatPacDot x = x



fieldWidth :: Float
fieldWidth = 50

windowWidth, windowHeight :: Float
windowWidth  = 650
windowHeight = 650
-- windowWidth, windowHeight :: Int
-- windowWidth  = 650
-- windowHeight = 650

levelWidth, levelHeight :: Int
levelWidth   = length (testLevel !! 0)
levelHeight  = length testLevel

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
           
testLevel, testEatenDots :: Level
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
testEatenDots = map (map eatPacDot) testLevel



data Item = PacDot
          | Fruit
          
data PacDot = Dot
            | PowerDot
            
instance Show PacDot where
    show Dot      = "D"
    show PowerDot = "P"
            
data Fruit = Float