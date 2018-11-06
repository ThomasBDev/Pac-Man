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
              

              
updatedLevelGhost :: Level -> Level
updatedLevelGhost level = updateGhost level ghostIndex (nextDirection level Chase ghostIndex)
                          where ghostIndex = selectCreature level G
                          
nextDirection :: Level -> Strategy -> Maybe Int -> Direction
nextDirection level Chase ghostIndex = shortestWay pacManPosition ghostPosition
                                     where pacManIndex    = selectCreature level S
                                           pacManPosition = currentPacManPosition pacManIndex
                                           ghostPosition  = currentPacManPosition ghostIndex
                                     
shortestWay :: Position -> Position -> Direction
                                                  --Pac-Man en Ghost zitten in dezelfde kolom.
shortestWay (Pos xPac yPac) (Pos xGhost yGhost) | xPac == xGhost = selectDirection 0 yPac yGhost                                                       
                                                  --Pac-Man en Ghost zitten in dezelfde rij.
                                                | yPac == yGhost = selectDirection 1 xPac xGhost
                                                  --Pac-Man en Ghost zitten niet op dezelfde kolom en rij.
                                                  -- Pac-Man is in het NE
                                                | (xPac > xGhost) && (yPac < yGhost) = North
                                                  -- Pac-Man is in het SE
                                                | (xPac > xGhost) && (yPac > yGhost) = East
                                                  -- Pac-Man is in het SW
                                                | (xPac < xGhost) && (yPac > yGhost) = South
                                                  -- Pac-Man is in het NW
                                                | (xPac < xGhost) && (yPac < yGhost) = West
                                                
selectDirection :: Int -> Int -> Int -> Direction
selectDirection 0 pac ghost | pac > ghost = South
                            | pac < ghost = North
selectDirection 1 pac ghost | pac > ghost = East
                            | pac < ghost = West
                                                
                                                
  
updateGhost :: Level -> Maybe Int -> Direction -> Level
updateGhost lvl Nothing _    = [[]]
updateGhost lvl (Just pos) d | checkWall lvl pos d = lvl
                             | otherwise           = singleToLevel (moveCreature singleList pos G d)
                             where singleList       = concat lvl
                                   singleToLevel xs = splitEvery levelWidth xs