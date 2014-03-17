module Main where

import Game

main :: IO ()
main = initGame >>= gameLoop
