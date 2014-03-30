
module Game where

import Snake
import System.Random
import Graphics.UI.GLUT hiding (Point)
import Data.IORef

data Status = Lose | Running deriving (Show, Eq)

type Score = Int

data Game = Game {
    snake :: Snake,
    board :: Board,
    status :: Status,
    fruit :: Fruit,
    score :: Score
} deriving (Show)

genFruit :: Game -> IO (Fruit)
genFruit game = do
    let w = (width $ board game) `div` 2
        h = (height $ board game) `div` 2
    ranX <- randomRIO (-w, w)
    ranY <- randomRIO (-h, h)
    let newFruit = Point ranX ranY
    if newFruit `elem` (body $ snake game) 
        then genFruit game 
        else return newFruit

isGotFruit :: Game -> Bool
isGotFruit (Game {fruit = f, snake = s}) = f == (head $ body s)

isDie :: Game -> Bool
isDie game = 
    let 
        shead = head $ body $ snake game
        collideBody = shead `elem` (tail $ body $ snake game)
        (Board w h) = board game
        isWall (Point x y) = abs x > (w `div` 2) || abs y > (h `div` 2)
    in
        or [collideBody, isWall shead]

initGame :: IO (IORef Game)
initGame = do 
    let game = Game {
        snake = Snake RIGHT [Point 0 0, Point (-1) 0, Point (-2) 0],
        board = Board 20 20,
        status = Running,
        fruit = Point 0 0,
        score = 0
    }
    f <- genFruit game
    newIORef $ game {fruit = f}

gameLoop :: IORef Game -> IO ()
gameLoop gameRef = do
    game <- readIORef gameRef
    let 
        dir = direction $ snake game
        snake' = advance dir $ snake game

    if (not.isDie) game {snake = snake'}
        then do
            let
                f = fruit game
                isGot = isGotFruit $ game {snake = snake'}
                nextScore = if isGot then (+) 10 else (+) 0
            nextFruit <- if isGot then genFruit game else return f
            writeIORef gameRef $ if isGot
                then game {snake = eatFruit f $ snake game, fruit = nextFruit, score = nextScore $ score game}
                else game {snake = advance dir $ snake game, score = nextScore $ score game}
            
            addTimerCallback 200 $ gameLoop gameRef
            postRedisplay Nothing
        else return ()

isOppsite :: Direction -> Direction -> Bool
isOppsite dir1 dir2 = case (dir1, dir2) of
    (LEFT, RIGHT) -> True
    (UP, DOWN)    -> True
    (RIGHT, LEFT) -> True
    (DOWN, UP)    -> True
    _             -> False

keyboardHandler :: IORef Game -> KeyboardMouseCallback
keyboardHandler gameRef key keyState _ _ = do
    case (key, keyState) of 
        (SpecialKey KeyUp,    Down) -> updateDir UP
        (SpecialKey KeyDown,  Down) -> updateDir DOWN
        (SpecialKey KeyLeft,  Down) -> updateDir LEFT
        (SpecialKey KeyRight, Down) -> updateDir RIGHT
        _                           -> return ()
    where
        updateDir dir = do
            game <- readIORef gameRef
            let origDir = direction $ snake game
            if isOppsite origDir dir
                then return ()
                else writeIORef gameRef $ game {snake = Snake dir $ body $ snake game}
