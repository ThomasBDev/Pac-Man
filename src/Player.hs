module Player where

import Level

lives :: Int
lives = 3

ghostCombo :: Int
ghostCombo = 0

score :: Int
score = 0

eatPacDot :: Int -> Int
eatPacDot s = s + 100

updatedLevel :: Level -> Direction -> Level
updatedLevel level dir = updatePacMan level pacManIndex dir
                       where pacManIndex = selectCreature level S
                       
-- Update de level array met de nieuwe positie van pac man
updatePacMan :: Level -> Maybe Int -> Direction -> Level
updatePacMan lvl Nothing _    = [[]]
updatePacMan lvl (Just pos) d | checkWall lvl pos d       = lvl
                              | checkTeleporter lvl pos d = singleToLevel (teleportPacMan singleList pos d)
                              | otherwise                 = singleToLevel (moveCreature singleList pos S d)
                              where singleList       = concat lvl
                                    singleToLevel xs = splitEvery levelWidth xs
                                    
teleportPacMan :: Row -> Int -> Direction -> Row
-- updatePacMan past de positie aan, dus dit gebeurt voordat Pac-Man op de teleporter komt.
-- Als de je naar een teleporter in het Noorden beweegt, dan ligt de nieuwe pacManIndex later in de array dan de oude.
-- North hier --> Gebruik South berekening.
teleportPacMan currentLevel pacManIndex North = replaceAtN1 (pacManIndex + (levelWidth * (levelHeight - 3))) pacManIndex S currentLevel
teleportPacMan currentLevel pacManIndex East  = replaceAtN2 (pacManIndex - (levelHeight - 1))                pacManIndex S currentLevel
teleportPacMan currentLevel pacManIndex South = replaceAtN2 (pacManIndex - (levelWidth * (levelHeight - 3))) pacManIndex S currentLevel
teleportPacMan currentLevel pacManIndex West  = replaceAtN1 (pacManIndex + (levelHeight - 1))                pacManIndex S currentLevel