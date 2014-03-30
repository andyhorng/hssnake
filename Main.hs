module Main where

import Game
import Draw
import Graphics.UI.GLUT hiding (Point)
import Data.IORef

main :: IO ()
-- main = initGame >>= gameLoop
main = do
    _ <- initialize "Snake" []
    initialDisplayMode $= [RGBAMode, DoubleBuffered]
    initialWindowSize  $= Size 800 800
    _ <- createWindow "Snake"

    gameRef <- initGame
    displayCallback $= display gameRef
    keyboardMouseCallback $= Just (keyboardHandler gameRef)
    gameLoop gameRef
    mainLoop

display :: IORef Game -> IO ()
display gameRef = do 
    game <- readIORef gameRef 
    draw game

