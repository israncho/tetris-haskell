module Board where

import Graphics.Gloss

-- | Lists of points to draw the grid of the board.
basePoints, ceilPoints, leftPoints, rightPoints :: [(Int, Int)]
basePoints = [(x * 30, 0) | x <- [0 .. 10], x * 30 `mod` 30 == 0]
ceilPoints = [(x * 30, 600) | x <- [0 .. 10], x * 30 `mod` 30 == 0]
leftPoints = [(0, x * 30) | x <- [0 .. 20], x * 30 `mod` 30 == 0]
rightPoints = [(300, x * 30) | x <- [0 .. 20], x * 30 `mod` 30 == 0]

myZipp :: [(Int, Int)] -> [(Int, Int)] -> [[(Float, Float)]]
myZipp [] _ = []
myZipp ((a, b) : xs) ((c, d) : ys) = [(iToF a, iToF b), (iToF c, iToF d)] : myZipp xs ys

iToF :: Int -> Float
iToF n = fromIntegral n :: Float

hLines = myZipp leftPoints rightPoints

vLines = myZipp basePoints ceilPoints

grey :: Color
grey = makeColor 0.5 0.5 0.5 1.0

-- | Given the lists of points returns the pictures to draw the grid.
drawGrid :: [[Point]] -> [Picture]
drawGrid = foldr ((:) . color grey . line) []

-- | Grid picture 
grid :: Picture
grid = pictures $ drawGrid (vLines ++ hLines)

-- | Function to draw a square in the board.
square :: (Float, Float) -> Color -> Picture
square (x, y) color_ = translate (x + 15) (y + 15) $ color color_ (rectangleSolid 30 30)
