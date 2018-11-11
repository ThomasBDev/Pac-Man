module Enemies where

import Level

data Ghost = Blinky Strategy
           | Inky   Strategy
           | Pinky  Strategy
           | Clyde  Strategy

data Strategy = Chase
              | FlankLeft
              | FlankRight
              | Random
              | Run
              | Return
              | Wait
              | Leave
   
-- Update a level based on a Ghost's actions.
updatedLevelGhost :: Level -> Int -> Level
updatedLevelGhost level randomIndex = updateGhost level ghostIndex (nextDirection level Chase ghostIndex randomIndex)
                                    where ghostIndex = selectCreature level G
   
-- Update a Ghost's position in a Level.   
updateGhost :: Level -> Maybe Int -> Direction -> Level
updateGhost lvl Nothing _    = [[]]
updateGhost lvl (Just pos) d | checkField lvl W pos d = lvl
                             | otherwise              = singleToLevel (moveCreature singleList pos G d)
                             where singleList       = concat lvl
                                   singleToLevel xs = splitEvery levelWidth xs
 
-- Give a Ghost a Direction to follow. 
nextDirection :: Level -> Strategy -> Maybe Int -> Int -> Direction
nextDirection level Chase ghostIndex randomIndex = shortestWay pacManPosition ghostPosition randomIndex
                                                 where pacManIndex    = selectCreature level S
                                                       pacManPosition = currentPacManPosition pacManIndex
                                                       ghostPosition  = currentPacManPosition ghostIndex
                                     
-- Get a Ghost with the Chase Strategy the shortest path to Pac-Man.
shortestWay :: Position -> Position -> Int -> Direction
                                                            -- Pac-Man and a Ghost are in the same row or column.
shortestWay (Pos xPac yPac) (Pos xGhost yGhost) randomIndex | xPac == xGhost = selectDirection 0 yPac yGhost
                                                            | yPac == yGhost = selectDirection 1 xPac xGhost
                                                            -- Pac-Man and a Ghost are not in the same row and column.
                                                            | (xPac > xGhost) && (yPac < yGhost) = [North, East] !! randomIndex
                                                            | (xPac > xGhost) && (yPac > yGhost) = [East, South] !! randomIndex
                                                            | (xPac < xGhost) && (yPac > yGhost) = [South, West] !! randomIndex
                                                            | (xPac < xGhost) && (yPac < yGhost) = [West, North] !! randomIndex

selectDirection :: Int -> Int -> Int -> Direction
selectDirection 0 pac ghost | pac > ghost = South
                            | pac < ghost = North
selectDirection 1 pac ghost | pac > ghost = East
                            | pac < ghost = West