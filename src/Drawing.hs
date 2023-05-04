module Drawing where

import Graphics.Gloss
import Tetromino

-- | Constants for the display and drawing
wWidth, wHeight, cellSize :: Int
wWidth = 200
wHeight = 400
cellSize = 20

-- | Constants for the display and drawing
halfWW, halfWH, cellSF, halfCSF :: Float
halfWW = fromIntegral wWidth / 2
halfWH = fromIntegral wHeight / 2
-- | Cell size 
cellSF = fromIntegral cellSize
-- | Half cell size 
halfCSF = cellSF / 2

-- | Lists of points to draw the grid of the board.
basePoints, ceilPoints, leftPoints, rightPoints :: [(Float, Float)]
basePoints = [(x * cellSF, 0) | x <- [0 .. 10]]
ceilPoints = [(x * cellSF, fromIntegral wHeight) | x <- [0 .. 10]]
leftPoints = [(0, x * cellSF) | x <- [0 .. 20]]
rightPoints = [(fromIntegral wWidth, x * cellSF) | x <- [0 .. 20]]

-- | Take two lists of points and returns a list of paths.
myZipp :: [(Float, Float)] -> [(Float, Float)] -> [[(Float, Float)]]
myZipp [] _ = []
myZipp ((a, b) : xs) ((c, d) : ys) = [(a, b), (c, d)] : myZipp xs ys

-- | Lists of paths to draw the grid.
hLines, vLines :: [[(Float, Float)]]
hLines = myZipp leftPoints rightPoints
vLines = myZipp basePoints ceilPoints

grey :: Color
grey = makeColor 0.5 0.5 0.5 1.0

-- | Given a list of paths(lines) returns the pictures to draw all the paths.
drawGrid :: [[Point]] -> [Picture]
drawGrid = foldr ((:) . color grey . line) []

-- | Grid picture
grid :: Picture
grid = pictures $ drawGrid (vLines ++ hLines)

-- | Function to draw a square in the display.
square :: (Float, Float) -> Color -> Picture
square (x, y) squcolr = translate (x + halfCSF) (y + halfCSF) $ color squcolr (rectangleSolid cellSF cellSF)

-- | Function to draw a square in the board given the cell position, color and way to draw.
boardSquare :: ((Float, Float) -> Color -> Picture) -> (Int, Int) -> Color -> Picture
boardSquare drawingFunction (x, y) color
  | x < 0 || x > 9 = error "Square out of board"
  | y < 0 || y > 19 = error "Square out of board"
  | otherwise = drawingFunction (fromIntegral x * cellSF, fromIntegral y * cellSF) color

-- | Returns the image of the tetromino to be drawn on the board.
-- Second parameter for the type of the tetromino.
drawTetromino :: Tetromino -> Bool -> Picture
drawTetromino t isGhost = pictures $ map (\position -> boardSquare drawingFunction position (tcolor t)) (cells t)
  where 
    drawingFunction = if isGhost then squareWire else square

-- | Function to draw the perimeter of a square in the display.
squareWire :: (Float, Float) -> Color -> Picture
squareWire (x, y) squcolr = translate (x + halfCSF) (y + halfCSF) $ color squcolr (rectangleWire (cellSF - 2) (cellSF - 2))
