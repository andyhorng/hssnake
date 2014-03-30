module Draw where

import Snake
import Game
import Graphics.UI.GLUT hiding (Point)

class Drawable a where
    draw :: a -> IO ()

unit ::GLfloat
unit = 0.05

conv :: Int -> GLfloat
conv a = (toFloat a) / 20 
    where toFloat = fromIntegral :: (Int -> GLfloat)

instance Drawable Point where
    draw (Point x y) = do
        renderPrimitive Quads $ do
            color  $ Color3 1 1 (1 :: GLfloat)
            vertex $ Vertex2 (vx+gap) (vy+gap)
            vertex $ Vertex2 (vx+gap) (vy+unit-gap)
            vertex $ Vertex2 (vx+unit-gap) (vy+unit-gap)
            vertex $ Vertex2 (vx+unit-gap) (vy+gap)
        where 
            vx = conv x
            vy = conv y
            gap = (unit / 10)

instance Drawable Board where
    draw b = do
        let w = conv $ (width b) `div` 2 
            h = conv $ (height b) `div` 2 
        renderPrimitive LineLoop $ do
            color  $ Color3 1 1 (0 :: GLfloat)
            vertex $ Vertex2 (-w) (h+unit)
            vertex $ Vertex2 (w+unit) (h+unit)
            vertex $ Vertex2 (w+unit) (-h)
            vertex $ Vertex2 (-w) (-h)

instance Drawable Snake where
    draw (Snake _ points) = mapM_ draw points

instance Drawable Game where
    draw game = do
        let 
            b = board game
            w = conv $ (width b) `div` 2 
        clear [ColorBuffer]
        color  $ Color3 1 0.0 (0 :: GLfloat)
        draw $ snake game
        draw $ board game
        draw $ fruit game
        renderText (w+(3*unit), 0) $ "Score: " ++ (show $ score game)
        swapBuffers

renderText :: (GLfloat, GLfloat) -> String -> IO ()
renderText (x,y) t = currentRasterPosition $= Vertex4 x y 0 1 >> renderString TimesRoman24 t
