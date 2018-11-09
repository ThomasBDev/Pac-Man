-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

import Level


-- Ik weet niet precies waar maar pacMouth is een waarde die moet worden geupdate bij animatie
wall, teleporter, home, dot, powerDot, pacMan, beginScreen, ghost :: Picture
wall        = color blue (rectangleSolid fieldWidth fieldWidth)
teleporter  = color orange (thickCircle (fieldWidth / 4) 10)
home        = color violet (arc 120 (-30) (fieldWidth / 2))
dot         = color aquamarine (thickCircle (fieldWidth / 10) 2)
powerDot    = color cyan (thickCircle (fieldWidth / 5) 5)
pacMan      = color yellow (thickArc 40 (-(pacMouth)) (fieldWidth / 5) 20)
ghost       = color red (rectangleSolid (fieldWidth / 2) (fieldWidth / 2))
                                          -- fromIntegral zet (o.a.) Int's om in Floats.
beginScreen = color green (rectangleSolid ((fromIntegral levelWidth) * fieldWidth) ((fromIntegral levelHeight) * fieldWidth))

-- thickCircle radius thickness


buildTile :: Float -> Float -> Field -> Picture
buildTile x y W = translate x y wall
buildTile x y T = translate x y teleporter
buildTile x y H = translate x y home
buildTile x y D = translate x y dot
buildTile x y P = translate x y powerDot
buildTile x y S = translate x y pacMan
buildTile x y G = translate x y ghost
buildTile _ _ _ = blank

buildRow :: Float -> Float -> Row -> [Picture]
buildRow _ _ []             = []
buildRow x y (field:fields) = buildTile x y field : buildRow (x + fieldWidth) y fields

buildLevel :: Float -> Float -> Level -> [[Picture]]
buildLevel _ _ []         = []
buildLevel x y (row:rows) = buildRow x y row : buildLevel x (y - fieldWidth) rows

constructedRow :: Picture
constructedRow = pictures (buildRow offsetX offsetY row13Walls)
               where offsetX = (-windowWidth / 2) + (fieldWidth / 2)
                     offsetY = (windowHeight / 2) - (fieldWidth / 2)

constructedLevel :: Level -> Picture
constructedLevel level = pictures (map pictures (buildLevel offsetX offsetY level))
                       where offsetX = (-windowWidth / 2) + (fieldWidth / 2)
                             offsetY = (windowHeight / 2) - (fieldWidth / 2)
 
{- 
variableLevel :: Picture
variableLevel = constructedLevel
-}

-- N = +y
-- E = +x


-- pacMan :: Picture
-- pacMan = color yellow (circleSolid 100)

-- -- spr_pacman.bmp heeft 300 x 300 pixels.
-- pacManSprite :: IO Picture
-- pacManSprite = loadBMP "spr_pacman.bmp"

-- loadBMP :: FilePath -> IO Picture

--           width  height ByteString
-- bitmap :: Int -> Int -> BitmapData -> Bool -> Picture



draw :: GameState -> IO Picture
draw = return . viewPure

-- infoToShow gstate == Title -> Create Picture with (F.E.) "PRESS START TO PLAY"
-- infoToShow gstate == Playing -> Create Picture with a Level.
-- infoToShow gstate == Paused -> Create Picture with "PAUSED" over the Level and the update method is disabled.
-- infoToShow gstate == GameOver -> Create Picture with "GAMEOVER".

--Geraakt worden door de ghost lijkt al een gameover te creeeren, waar staat dit?
viewPure :: GameState -> Picture
viewPure gstate = case typeOfState gstate of
  Title         -> beginScreen
  Playing       -> constructedLevel (currentLevel gstate)  --variableLevel + translated pacMan = huidige levelstate?
-- color :: Color -> Picture -> Picture

-- text :: String -> Picture