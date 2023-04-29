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

-- | Display of the tetris.
tetrisDisplay :: Display
tetrisDisplay = InWindow "Tetris" (wWidth + 1, wHeight + 1) (200, 200)

-- | Given the lists of points returns the pictures to draw the grid.
drawGrid :: [[Point]] -> [Picture]
drawGrid = foldr ((:) . color grey . line) []

-- | Grid picture 
grid :: Picture
grid = pictures $ drawGrid (vLines ++ hLines)

-- | Function to draw a square in the board.
square :: (Float, Float) -> Color -> Picture
square (x, y) color_ = translate (x + 15) (y + 15) $ color color_ (rectangleSolid 30 30)

drawBoard :: Picture
drawBoard = translate (-halfWW) (-halfWH) (pictures [square (30, 30) red, grid])

main :: IO ()
main =
  display
    tetrisDisplay
    black
    drawBoard