module Snake where

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

advance :: Direction -> Snake -> Snake
advance dir (Snake _ points) = case dir of
        UP    -> Snake UP $ (Point hx (hy+1)):(init points)
        DOWN  -> Snake DOWN $ (Point hx (hy-1)):(init points)
        LEFT  -> Snake LEFT $ (Point (hx-1) hy):(init points)
        RIGHT -> Snake RIGHT $ (Point (hx+1) hy):(init points)
    where 
        (Point hx hy) = head points

eatFruit :: Fruit -> Snake -> Snake
eatFruit f s = s {body = f:(body s)}
