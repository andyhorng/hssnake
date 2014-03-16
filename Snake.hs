module Snake
where

import qualified UI.HSCurses.Curses as HC
import System.IO
import Control.Concurrent(threadDelay)

data Direction = LEFT | RIGHT | UP | DOWN deriving (Eq, Show)
data Point = Point Int Int deriving (Eq, Show)
data Board = Board {
    width :: Int, 
    height :: Int
} deriving (Show)
data Snake = Snake {
    direction :: Direction,
    body      :: [Point]
} deriving (Show)

class Drawable a where
    draw :: a -> IO ()

putPoint x y c = HC.mvWAddStr HC.stdScr x y c

instance Drawable Point where
    draw (Point x y) = putPoint x y $ ['*']

instance Drawable Board where
    draw (Board w h) = do
        mapM_ draw points
        where points = [Point 0 y | y <- [0..w-1]] ++ 
                       [Point x 0 | x <- [0..h-1]] ++ 
                       [Point (h-1) y | y <- [0..w-1]] ++ 
                       [Point x (w-1) | x <- [0..h-1]]

instance Drawable Snake where
    draw (Snake _ points) = mapM_ draw points

advance :: Direction -> Snake -> Snake
advance direction s@(Snake origDir points) 
    | isAllow direction origDir = case direction of
        UP -> Snake UP $ (Point (hx-1) hy):(init points)
        DOWN -> Snake DOWN $ (Point (hx+1) hy):(init points)
        LEFT -> Snake LEFT $ (Point hx (hy-1)):(init points)
        RIGHT -> Snake RIGHT $ (Point hx (hy+1)):(init points)
    | otherwise = advance direction s
    where 
        (Point hx hy) = head points
        isAllow UP DOWN = False
        isAllow DOWN UP = False
        isAllow RIGHT LEFT = False
        isAllow LEFT RIGHT = False
        isAllow _ _ = True

data Game = Game {
    snake :: Snake,
    board :: Board
}

instance Drawable Game where
    draw game = do
        HC.wclear HC.stdScr
        draw $ snake game
        draw $ board game
        HC.refresh

gameLoop :: Game -> IO ()
gameLoop game = do
    draw game
    threadDelay 200000
    gameLoop game {snake = advance RIGHT $ snake game}

main :: IO ()
main = do 
    hSetBuffering stdin NoBuffering
    HC.initCurses
    _ <- HC.leaveOk True
    HC.echo False
    let game = Game {
        snake = Snake RIGHT [Point 3 5, Point 3 4, Point 3 3, Point 3 2],
        board = Board 30 30 
    }
    gameLoop game
    HC.endWin
