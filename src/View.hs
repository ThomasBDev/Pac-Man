-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

draw :: GameState -> IO Picture
draw = return . viewPure

-- infoToShow gstate == Title -> Create Picture with (F.E.) "PRESS START TO PLAY"
-- infoToShow gstate == Playing -> Create Picture with a Level.
-- infoToShow gstate == Paused -> Create Picture with "PAUSED" over the Level and the update method is disabled.
-- infoToShow gstate == GameOver -> Create Picture with "GAMEOVER".

viewPure :: GameState -> Picture
viewPure gstate = case infoToShow gstate of
  ShowNothing   -> blank
  ShowANumber n -> color green (text (show n))
  ShowAChar   c -> color green (text [c])
  
-- color :: Color -> Picture -> Picture

-- text :: String -> Picture