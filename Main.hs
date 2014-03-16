module Main where

import Snake

main :: IO ()
main = do 
    let game = Game {
        snake = Snake RIGHT [Point 3 5, Point 3 4, Point 3 3, Point 3 2],
        board = Board 30 30 ,
        status = Running,
        fruit = Point 0 0
    }
    f <- genFruit game

    let withFruit = game {fruit = f}

    gameLoop withFruit
