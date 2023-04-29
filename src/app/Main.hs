module Main where

import Board
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

grey :: Color
grey = makeColor 0.5 0.5 0.5 1.0

-- | Given the lists of points returns the picture to draw the grid.
drawGrid :: [[Point]] -> [Picture]
drawGrid = foldr ((:) . color grey . line) []

grid = drawGrid (vLines ++ hLines)

main :: IO ()
main =
  display
    (InWindow "Tetris" (wWidth+1, wHeight+1) (50, 50))
    black
    (translate (-halfWW) (-halfWH ) (pictures grid))
