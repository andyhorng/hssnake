
module Game where

import Snake
import System.Random
import UI.NCurses as NC
import Data.List

data Status = Lose | Running deriving (Show, Eq)

data Game = Game {
    snake :: Snake,
    board :: Board,
    status :: Status,
    fruit :: Fruit
}

instance Drawable Game where
    draw game = do
        -- clear manually
        mapM_ (\(Point x y) -> putPoint (toInteger x) (toInteger y) " ") $ getIdlePoints game
        draw $ snake game
        draw $ board game
        draw $ fruit game

getIdlePoints:: Game -> [Point]
getIdlePoints game = 
    let 
        b@(Board w h) = board game
        s = snake game
        allPoints = [Point x y | x <- [0..w-1], y <- [0..h-1]]
    in
        allPoints \\ (getBoardPoints b ++ body s ++ [fruit game])

genFruit :: Game -> IO (Fruit)
genFruit game = randomRIO (0, length idles - 1) >>= return . (idles !!)
    where idles = getIdlePoints game

hitFruit :: Game -> Bool
hitFruit (Game {fruit = f, snake = s}) = f == (head $ body s)

getCommand :: NC.Window -> NC.Curses (Maybe Direction)
getCommand w = do
    event <- NC.getEvent w $ Just 200
    return $ case event of
        (Just (EventSpecialKey KeyUpArrow)) -> Just UP
        (Just (EventSpecialKey KeyDownArrow)) -> Just DOWN
        (Just (EventSpecialKey KeyLeftArrow)) -> Just LEFT
        (Just (EventSpecialKey KeyRightArrow)) -> Just RIGHT
        _ -> Nothing

initGame :: IO Game 
initGame = do 
    let game = Game {
        snake = Snake RIGHT [Point 3 5],
        board = Board 30 30 ,
        status = Running,
        fruit = Point 0 0
    }
    f <- genFruit game
    return game {fruit = f}

gameLoop :: Game -> IO ()
gameLoop game@(Game {status = Running}) = 
    if hitFruit game then do
        newFruit <- genFruit game
        let game' = game {
            snake = eatFruit (fruit game) (snake game),
            fruit = newFruit
        }
        gameLoop game'
    else do 
        command <- NC.runCurses $ do 
            w <- NC.defaultWindow
            NC.updateWindow w $ do
                draw game
            NC.render
            getCommand w

        case command of
            Just dir -> gameLoop game {snake = advance dir $ snake game}
            Nothing -> gameLoop game {snake = advance origDir $ snake game}

    where 
        origDir = direction $ snake game

-- Lose
gameLoop (Game {status = Lose}) = return ()
