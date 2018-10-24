module Main where

import Model
import View
import Controller

import Level

import Graphics.Gloss.Interface.IO.Game

main :: IO()

-- main = printLevel testLevel

main = playIO displayWindow -- Or FullScreen
              black            -- Background color
              10               -- Frames per second
              initialState    -- Initial state
              draw             -- View function
              input            -- Event function
              update             -- Step function
              
displayWindow :: Display
displayWindow = InWindow "Pac Man" (windowWidth, windowHeight) (100, 100)

-- step functie wordt elke frame aangeroepen.

{-
playIO :: Display ->
          Color ->
          Int ->
          world ->
          (world -> IO Picture) ->
          (Event -> world -> IO world) -> 
          (Float -> world -> IO world) ->
          IO ()
-}