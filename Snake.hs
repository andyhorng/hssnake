module Snake
where

import UI.NCurses as NC

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

type Fruit = Point
--
class Drawable a where
    draw :: a -> NC.Update ()

--
putPoint :: Integer -> Integer -> String -> NC.Update ()
putPoint x y str = NC.moveCursor x y >> NC.drawString str

instance Drawable Point where
    draw (Point x y) = putPoint (toInteger x) (toInteger y) $ "*"

instance Drawable Board where
    draw b = do
        mapM_ draw $ getBoardPoints b

instance Drawable Snake where
    draw (Snake _ points) = mapM_ draw points

getBoardPoints :: Board -> [Point]
getBoardPoints (Board w h) = 
    [Point 0 y | y <- [0..w-1]] ++ 
    [Point x 0 | x <- [0..h-1]] ++ 
    [Point (h-1) y | y <- [0..w-1]] ++ 
    [Point x (w-1) | x <- [0..h-1]]

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

eatFruit :: Fruit -> Snake -> Snake
eatFruit f s = s {body = f:(body s)}
