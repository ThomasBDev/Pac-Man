-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

import Level
import Player


-- Ik weet niet precies waar maar pacMouth is een waarde die moet worden geupdate bij animatie
wall, teleporter, home, dot, powerDot, pacMan, ghost, titleScreen, pausedScreen, gameOverScreen :: Picture
wall        = color blue (rectangleSolid fieldWidth fieldWidth)
teleporter  = color orange (thickCircle (fieldWidth / 4) 10)
home        = color violet (arc 120 (-30) (fieldWidth / 2))
dot         = color aquamarine (thickCircle (fieldWidth / 10) 2)
powerDot    = color cyan (thickCircle (fieldWidth / 5) 5)
pacMan      = color yellow (thickArc pacMouth (-pacMouth) (fieldWidth / 5) 20)
ghost       = color red (rectangleSolid (fieldWidth / 2) (fieldWidth / 2))
                                          -- fromIntegral zet (o.a.) Int's om in Floats.
                                          
titleScreen    = color blue (rectangleSolid ((fromIntegral levelWidth) * fieldWidth) ((fromIntegral levelHeight) * fieldWidth))
pausedScreen   = color magenta (translate ((-windowWidth / 2) + 80) 0 (text "PAUSED"))
gameOverScreen = color white (rectangleSolid ((fromIntegral levelWidth) * fieldWidth) ((fromIntegral levelHeight) * fieldWidth))
                             
constructBeginScreen :: Picture
constructBeginScreen = pictures [titleScreen, pacMan, titletext, begintext, highscoretext]

scoretext, highscoretext, begintext, titletext, pausetext, gameovertext :: Picture
scoretext = translate 0 (-270) (scale 0.2 0.2 (color red (text "Score: ")))
highscoretext = translate (-300) (-270) (scale 0.2 0.2 (color red (text "High-Score:")))
begintext = translate (-170) (-160) (scale 0.2 0.2 (color red (text "Press SPACEBAR to start!")))
titletext = translate (-170) 160 (scale 0.2 0.2 (color red (text "PAC-MAN IN HASKELLAND" )))
pausetext = color red (text "Pause")
gameovertext = color red (text "Game over")

constructPausedScreen :: GameState -> Picture
constructPausedScreen gstate = pictures [(constructLevel gstate), pausedScreen]
                  
                  

buildTile :: Float -> Float -> Field -> Picture
buildTile x y W = translate x y wall
buildTile x y T = translate x y teleporter
buildTile x y H = translate x y home
buildTile x y D = translate x y dot
buildTile x y P = translate x y powerDot
-- buildTile x y S = translate x y pacMan
buildTile x y G = translate x y ghost
buildTile _ _ _ = blank

buildPacMan :: Float -> Float -> Float -> Picture
buildPacMan x y 0        = translate x y pacMan
buildPacMan x y variable = translate x y pacManPicture
                         where pacManPicture = color yellow (thickArc (variable * 60) (-variable * 60) (fieldWidth / 5) 20)
                                                            -- thickArc wordt tegen de klok in getekend.
                                                            -- thickCircle radius thickness

buildRow :: Float -> Float -> Float -> Row -> [Picture]
buildRow _ _ _ []                    = []
buildRow x y variable (S:fields)     = buildPacMan x y variable : buildRow (x + fieldWidth) y variable fields
buildRow x y variable (field:fields) = buildTile x y field      : buildRow (x + fieldWidth) y variable fields

buildLevel :: Float -> Float -> Float -> Level -> [[Picture]]
buildLevel _ _ _ []                = []
buildLevel x y variable (row:rows) = buildRow x y variable row : buildLevel x (y - fieldWidth) variable rows

constructLevel :: GameState -> Picture
constructLevel currentGameState = pictures (scoretext : highscoretext: (map pictures (buildLevel offsetX offsetY (elapsedTime currentGameState) (currentLevel currentGameState))))
                                  where offsetX = (-windowWidth / 2) + (fieldWidth / 2)
                                        offsetY = (windowHeight / 2) - (fieldWidth / 2)
 


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

viewPure :: GameState -> Picture
viewPure gstate = case typeOfState gstate of
  Title         -> constructBeginScreen
  Playing       -> constructLevel gstate
  Paused        -> constructPausedScreen gstate
  GameOver      -> gameOverScreen