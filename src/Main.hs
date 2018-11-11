module Main where

import Model
import View
import Controller

import Level

import Graphics.Gloss.Interface.IO.Game

main :: IO()
main = playIO displayWindow    -- Window or FullScreen
              black            -- Background color
              10               -- Frames per second
              initialState     -- Initial state
              draw             -- View function
              input            -- Event function
              update           -- Step function
              
displayWindow :: Display
displayWindow = InWindow "Pac Man" (round windowWidth, round windowHeight) (100, 100)