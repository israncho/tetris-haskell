module Main where

import Graphics.Gloss

wWidth, wHeight, cellSize :: Int

-- | window width
wWidth = 300

-- | window height
wHeight = 600

-- | cell size
cellSize = 30

halfWW, halfWH :: Float

-- | half of the width
halfWW = fromIntegral wWidth / 2

-- | half of the height
halfWH = fromIntegral wHeight / 2

gridWidth, gridHeight :: Int
gridWidth = 10
gridHeight = 20

grey = makeColor 0.5 0.5 0.5 1.0

drawingFunction :: Picture
drawingFunction = translate (-50) 50 $ color red $ circle 80

grid :: [[Point]] -> [Picture]
grid [] = [] 
grid ([p1, p2] : xs) = color grey (line [p1, p2]) : grid xs

main :: IO ()
main =
  display
    (InWindow "Tetris" (wWidth, wHeight) (50, 50))
    black
    (translate (-halfWW) (-halfWH) (pictures $ grid [[(0,0),(0,600)],[(30,0),(30,600)]]))
