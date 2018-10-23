module Enemies where

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