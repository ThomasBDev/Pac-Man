-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

import Level

wall, teleporter, home :: Picture
wall       = color blue (rectangleSolid fieldWidth fieldWidth)
teleporter = color orange (thickCircle 5 (fieldWidth / 2))
home       = color green (arc 30 (-30) (fieldWidth / 2))

pacMan :: Picture
pacMan = color yellow (circleSolid 100)

-- spr_pacman.bmp heeft 300 x 300 pixels.
pacManSprite :: IO Picture
pacManSprite = loadBMP "spr_pacman.bmp"

-- loadBMP :: FilePath -> IO Picture

--           width  height ByteString
-- bitmap :: Int -> Int -> BitmapData -> Bool -> Picture



draw :: GameState -> IO Picture
draw = return . viewPure

-- infoToShow gstate == Title -> Create Picture with (F.E.) "PRESS START TO PLAY"
-- infoToShow gstate == Playing -> Create Picture with a Level.
-- infoToShow gstate == Paused -> Create Picture with "PAUSED" over the Level and the update method is disabled.
-- infoToShow gstate == GameOver -> Create Picture with "GAMEOVER".

viewPure :: GameState -> Picture
viewPure gstate = case infoToShow gstate of
  ShowNothing   -> pacMan
  ShowANumber n -> color blue (text (show n))
  ShowAChar   c -> color green (text [c])
  
-- color :: Color -> Picture -> Picture

-- text :: String -> Picture