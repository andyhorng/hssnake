module Snake
where

import UI.NCurses as NC
import Data.List
import System.Random

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
    draw :: a -> NC.Update ()

putPoint :: Integer -> Integer -> String -> NC.Update ()
putPoint x y str = NC.moveCursor x y >> NC.drawString str

instance Drawable Point where
    draw (Point x y) = putPoint (toInteger x) (toInteger y) $ "*"

instance Drawable Board where
    draw b = do
        mapM_ draw $ getBoardPoints b

getBoardPoints :: Board -> [Point]
getBoardPoints (Board w h) = 
    [Point 0 y | y <- [0..w-1]] ++ 
    [Point x 0 | x <- [0..h-1]] ++ 
    [Point (h-1) y | y <- [0..w-1]] ++ 
    [Point x (w-1) | x <- [0..h-1]]

instance Drawable Snake where
    draw (Snake _ points) = mapM_ draw points

advance :: Direction -> Snake -> Snake
advance dir s@(Snake origDir points) 
    | isAllow dir origDir = case dir of
        UP    -> Snake UP $ (Point (hx-1) hy):(init points)
        DOWN  -> Snake DOWN $ (Point (hx+1) hy):(init points)
        LEFT  -> Snake LEFT $ (Point hx (hy-1)):(init points)
        RIGHT -> Snake RIGHT $ (Point hx (hy+1)):(init points)
    | otherwise = advance origDir s 
    where 
        (Point hx hy) = head points
        isAllow UP DOWN = False
        isAllow DOWN UP = False
        isAllow RIGHT LEFT = False
        isAllow LEFT RIGHT = False
        isAllow _ _ = True

data Status = HitFruit | Loose | Running

type Fruit = Point

data Game = Game {
    snake :: Snake,
    board :: Board,
    status :: Status,
    fruit :: Fruit
}

getIdlePoints:: Game -> [Point]
getIdlePoints game = 
    let 
        b@(Board w h) = board game
        s = snake game
        allPoints = [Point x y | x <- [0..w-1], y <- [0..h-1]]
    in
        allPoints \\ (getBoardPoints b ++ body s ++ [fruit game])

instance Drawable Game where
    draw game = do
        -- clear manually
        mapM_ (\(Point x y) -> putPoint (toInteger x) (toInteger y) " ") $ getIdlePoints game
        draw $ snake game
        draw $ board game
        draw $ fruit game

genFruit :: Game -> IO (Fruit)
genFruit game = randomRIO (0, length idles - 1) >>= return . (idles !!)
    where idles = getIdlePoints game

getCommand :: NC.Window -> NC.Curses (Maybe Direction)
getCommand w = do
    event <- NC.getEvent w $ Just 200
    return $ case event of
        (Just (EventSpecialKey KeyUpArrow)) -> Just UP
        (Just (EventSpecialKey KeyDownArrow)) -> Just DOWN
        (Just (EventSpecialKey KeyLeftArrow)) -> Just LEFT
        (Just (EventSpecialKey KeyRightArrow)) -> Just RIGHT
        _ -> Nothing

gameLoop :: Game -> IO ()
gameLoop game = do
    command <- NC.runCurses $ do 
        w <- NC.defaultWindow
        NC.updateWindow w $ do
            draw game
        NC.render
        getCommand w

    case command of
        Just dir -> gameLoop game {snake = advance dir $ snake game}
        Nothing -> gameLoop game {snake = advance origDir $ snake game}

    where origDir = direction $ snake game
