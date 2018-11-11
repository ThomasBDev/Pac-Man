module Player where

import Level

lives :: Int
lives = 3

ghostCombo :: Int
ghostCombo = 0

pacMouth :: Float
pacMouth = 40

readNumberFromFile :: FilePath -> IO Int
readNumberFromFile path = do
   content <- readFile path
   return (read content)
   
loadScore :: IO Int
loadScore = readNumberFromFile "highscore.txt"

saveScore :: Int -> IO ()
saveScore score = writeFile "highscore.txt" (show score)

-- Add points to the player's score when he eats a dot.
-- Otherwise, keep the score the same.
updatedScore :: Bool -> Int -> Int
updatedScore False score = score
updatedScore True score = score + 100

-- Update a level based on the player's actions.
updatedLevel :: Level -> Direction -> Level
updatedLevel level dir = updatePacMan level pacManIndex dir
                       where pacManIndex = selectCreature level S
                       
-- Update Pac-Man's position in a Level.
updatePacMan :: Level -> Maybe Int -> Direction -> Level
updatePacMan lvl Nothing _    = [[]]
updatePacMan lvl (Just pos) d | checkField lvl W pos d = lvl
                              | checkField lvl T pos d = singleToLevel (teleportPacMan singleList pos d)
                              | otherwise              = singleToLevel (moveCreature singleList pos S d)
                              where singleList       = concat lvl
                                    singleToLevel xs = splitEvery levelWidth xs
                                    
teleportPacMan :: Row -> Int -> Direction -> Row
teleportPacMan currentLevel pacManIndex North = replaceAtN1 (pacManIndex + (levelWidth * (levelHeight - 3))) pacManIndex S currentLevel
teleportPacMan currentLevel pacManIndex East  = replaceAtN2 (pacManIndex - (levelHeight - 1))                pacManIndex S currentLevel
teleportPacMan currentLevel pacManIndex South = replaceAtN2 (pacManIndex - (levelWidth * (levelHeight - 3))) pacManIndex S currentLevel
teleportPacMan currentLevel pacManIndex West  = replaceAtN1 (pacManIndex + (levelHeight - 1))                pacManIndex S currentLevel