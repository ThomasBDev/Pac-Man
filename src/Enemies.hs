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
              

              
updatedLevelGhost :: Level -> Direction -> Level
updatedLevelGhost level dir = updateGhost level ghostIndex dir
                            where ghostIndex = selectCreature level G
  
updateGhost :: Level -> Maybe Int -> Direction -> Level
updateGhost lvl Nothing _    = [[]]
updateGhost lvl (Just pos) d | checkWall lvl pos d = lvl
                             | otherwise           = singleToLevel (moveCreature singleList pos G d)
                             where singleList       = concat lvl
                                   singleToLevel xs = splitEvery levelWidth xs