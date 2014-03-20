
module Game where

import Snake
import System.Random
import UI.NCurses as NC
import Data.List

data Status = Lose | Running deriving (Show, Eq)

type Score = Int

data Game = Game {
    snake :: Snake,
    board :: Board,
    status :: Status,
    fruit :: Fruit,
    score :: Score
}

instance Drawable Game where
    draw game = do
        -- clear manually
        mapM_ (\(Point x y) -> putPoint (toInteger x) (toInteger y) " ") $ getIdlePoints game
        draw $ snake game
        draw $ board game
        let (Point fx fy) = fruit game
        putPoint (toInteger fx) (toInteger fy) "."

        let (Board w h) = board game
        putPoint (toInteger (h `div` 2)) (toInteger (w+1)) $ "Score: " ++ (show . score) game

getIdlePoints:: Game -> [Point]
getIdlePoints game = 
    let 
        (Board w h) = board game
        allPoints = [Point x y | x <- [0..w-1], y <- [0..h-1]]
    in
        nub (allPoints \\ getWorkingPoints game)

getWorkingPoints :: Game -> [Point]
getWorkingPoints game = 
    let
        b = board game
        s = snake game
    in
        nub $ getBoardPoints b ++ body s ++ [fruit game]

genFruit :: Game -> IO (Fruit)
genFruit game = randomRIO (0, length idles - 1) >>= return . (idles !!)
    where idles = getIdlePoints game

hitFruit :: Game -> Bool
hitFruit (Game {fruit = f, snake = s}) = f == (head $ body s)

collide :: Game -> Bool
collide game = 
    let 
        shead = head $ body $ snake game
        collideBody = shead `elem` (tail $ body $ snake game)
        collideBoard = shead `elem` (getBoardPoints $ board game)
    in
        or [collideBody, collideBoard]

getCommand :: NC.Window -> NC.Curses (Maybe Direction)
getCommand w = do
    event <- NC.getEvent w $ Just 500
    return $ case event of
        (Just (EventSpecialKey KeyUpArrow)) -> Just UP
        (Just (EventSpecialKey KeyDownArrow)) -> Just DOWN
        (Just (EventSpecialKey KeyLeftArrow)) -> Just LEFT
        (Just (EventSpecialKey KeyRightArrow)) -> Just RIGHT
        _ -> Nothing

initGame :: IO Game 
initGame = do 
    let game = Game {
        snake = Snake RIGHT [Point 2 2],
        board = Board 30 30,
        status = Running,
        fruit = Point 0 0,
        score = 0
    }
    f <- genFruit game
    return game {fruit = f}

gameLoop :: Game -> IO ()
gameLoop game@(Game {status = Running}) 

    | collide game = do
        gameLoop (game {status = Lose})

    | otherwise = do
        command <- NC.runCurses $ do 
            w <- NC.defaultWindow
            NC.updateWindow w $ do
                draw game
            NC.render
            getCommand w

        let 
            newDir = case command of
                    Just dir -> dir
                    Nothing -> direction $ snake game
            newSnake = advance newDir $ snake game
            newGame = game {snake = newSnake}

        if hitFruit newGame
            then  do
                newFruit <- genFruit game
                let snake' = (eatFruit (fruit game) (snake game)) {direction = newDir}
                gameLoop game {
                    snake = snake',
                    fruit = newFruit, 
                    score = (score game) + 1
                }
            else 
                gameLoop newGame

-- Lose
gameLoop game@(Game {status = Lose}) = 
    putStrLn $ "Your Score: " ++ (show $ score game)
