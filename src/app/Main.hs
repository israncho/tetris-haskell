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

drawingFunction :: Picture
drawingFunction = translate (-50) 50 $ color red $ circle 80

linePicture :: [Point] -> Picture
linePicture points = color red (line points)

main :: IO ()
main =
  display
    (InWindow "Tetris" (wWidth, wHeight) (50, 50))
    black
    (translate (-halfWW) (-halfWH) (linePicture [(0, 0), (100, 100)]))