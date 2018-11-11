-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

import Level
import Player

-- The sprites for the objects in a level.
wall, teleporter, dot, pacMan, ghost :: Picture
wall        = color blue (rectangleSolid fieldWidth fieldWidth)
teleporter  = color orange (thickCircle (fieldWidth / 4) 10)
dot         = color aquamarine (thickCircle (fieldWidth / 10) 2)
pacMan      = color yellow (thickArc pacMouth (-pacMouth) (fieldWidth / 5) 20)
ghost       = color red (rectangleSolid (fieldWidth / 2) (fieldWidth / 2))

-- A Picture to display the player's score for the current level.
scoretext :: Int -> Picture
scoretext score = translate 0 (-270) (scale 0.2 0.2 (color red (text ("Score: " ++ show score))))

-- A Picture to display the current highscore.
highscoretext :: GameState -> Picture
highscoretext gstate | highscore > score = translate (-300) (-270) (scale 0.2 0.2 (color red (text ("High-Score: " ++ show highscore))))
                     | otherwise         = translate (-300) (-270) (scale 0.2 0.2 (color red (text ("High-Score: " ++ show score))))
                     where highscore = currentHighScore gstate
                           score     = currentScore gstate

-- Pictures to display various texts through the game.
begintext, randomtext, titletext, pausetext, gameovertext, returntext, optionstext :: Picture
begintext    = translate (-270) (-160) (scale 0.2 0.2 (color red (text "LEVEL SELECT: Press 1, 2, 3 or 4!")))
randomtext   = translate (-270) (-190) (scale 0.2 0.2 (color red (text "RANDOM SELECT: Press R!")))
titletext    = translate (-170) 160    (scale 0.2 0.2 (color red (text "PAC-MAN IN HASKELLAND" )))
pausetext    = color red (text "Pause")
gameovertext = translate (-70)  0      (scale 0.2 0.2 (color red (text "Game over")))
returntext   = translate (-270) (-160) (scale 0.2 0.2 (color red (text "Press SPACEBAR to go return to title")))
optionstext  = translate (-300) (-300) (scale 0.2 0.2 (color red (text "PAUSE: Press P!    QUIT: Press Q!")))

-- Pictures that display information for the Title and Paused states.
constructBeginScreen, constructPausedScreen :: GameState -> Picture
constructBeginScreen gstate  = pictures [titleScreen, pacMan, titletext, begintext, randomtext, (highscoretext gstate)]
constructPausedScreen gstate = pictures [(constructLevel gstate), pausedScreen] 

-- Pictures that contain information for various states.
titleScreen, pausedScreen, gameOverScreen, constructGameOverScreen :: Picture
titleScreen             = color blue (rectangleSolid ((fromIntegral levelWidth) * fieldWidth) ((fromIntegral levelHeight) * fieldWidth))
pausedScreen            = color magenta (translate ((-windowWidth / 2) + 80) 0 (text "PAUSED"))
gameOverScreen          = color blue (rectangleSolid ((fromIntegral levelWidth) * fieldWidth) ((fromIntegral levelHeight) * fieldWidth))
constructGameOverScreen = pictures [gameOverScreen, gameovertext, returntext]



-- Based on the position and field, display a sprite of an object.
buildTile :: Float -> Float -> Field -> Float -> Picture
buildTile x y W _   = translate x y wall
buildTile x y T 0   = translate x y teleporter
buildTile x y T var = translate x y teleporterPicture
                           where teleporterPicture = color orange (thickCircle (var * 20) 10)
buildTile x y D _   = translate x y dot
buildTile x y S 0   = translate x y pacMan
buildTile x y S var = translate x y pacManPicture
                           where pacManPicture = color yellow (thickArc (var * 60) (-var * 60) (fieldWidth / 5) 20)
buildTile x y G _   = translate x y ghost
buildTile _ _ _ _   = blank

-- A function that executes buildTile for each element in a Row.
buildRow :: Float -> Float -> Float -> Row -> [Picture]
buildRow _ _ _ []               = []
buildRow x y var (field:fields) = buildTile x y field var : buildRow (x + fieldWidth) y var fields

-- A function that executes buildRow for each Row in a Level.
buildLevel :: Float -> Float -> Float -> Level -> [[Picture]]
buildLevel _ _ _ []           = []
buildLevel x y var (row:rows) = buildRow x y var row : buildLevel x (y - fieldWidth) var rows

-- Create a Picture off all the elements in a level, based on the current values in the GameState
constructLevel :: GameState -> Picture
constructLevel currentGameState = pictures ((scoretext (currentScore currentGameState)) : (highscoretext currentGameState) : optionstext : (map pictures (buildLevel offsetX offsetY (elapsedTime currentGameState) (currentLevel currentGameState))))
                                  where offsetX = (-windowWidth / 2) + (fieldWidth / 2)
                                        offsetY = (windowHeight / 2) - (fieldWidth / 2)

draw :: GameState -> IO Picture
draw = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = case typeOfState gstate of
  Title         -> constructBeginScreen gstate
  Playing       -> constructLevel gstate
  Paused        -> constructPausedScreen gstate
  GameOver      -> constructGameOverScreen