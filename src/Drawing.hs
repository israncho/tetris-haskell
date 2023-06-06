module Drawing where

import Board (Board, Cell (cellColor, position), upMv)
import Graphics.Gloss
import Tetromino

-- | Constants for the display and drawing
boardWidth, boardHeight, cellSize, wHeight, wWidth :: Int
boardWidth = 300
boardHeight = 600
cellSize = 30
wHeight = boardHeight
wWidth = boardWidth + (cellSize * 8)

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
ceilPoints = [(x * cellSF, fromIntegral boardHeight) | x <- [0 .. 10]]
leftPoints = [(0, x * cellSF) | x <- [0 .. 20]]
rightPoints = [(fromIntegral boardWidth, x * cellSF) | x <- [0 .. 20]]

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

-- | Function to draw a square outside the board given the cell position, color and way to draw.
squareOutsideBoard :: ((Float, Float) -> Color -> Picture) -> (Int, Int) -> Color -> Picture
squareOutsideBoard drawingFunction (x, y) color
  | 0 < x && x < 9 && 0 < y && y < 19 = error "Square inside the board"
  | otherwise = drawingFunction (fromIntegral x * cellSF, fromIntegral y * cellSF) color

-- | Returns the image of the tetromino to be drawn on the board.
-- Second parameter for the type of the tetromino.
drawTetromino :: Tetromino -> Bool -> Picture
drawTetromino t isGhost = pictures $ map (\position -> boardSquare drawingFunction position (tcolor t)) (tcells t)
  where
    drawingFunction = if isGhost then squareWire else square

-- | Function to draw the perimeter of a square in the display.
squareWire :: (Float, Float) -> Color -> Picture
squareWire (x, y) squcolr = translate (x + halfCSF) (y + halfCSF) $ color squcolr (pictures 
  [rectangleWire (cellSF - 2) (cellSF - 2), rectangleWire (cellSF - 4) (cellSF - 4)])

-- | Squares of the background of the board.
backgroundSquares :: [(Int, Int)]
backgroundSquares = [(x, y) | x <- [0 .. 9], y <- [0 .. 19]]

-- | Background of the board.
boardBackground :: Picture
boardBackground = pictures $ map (\coord -> boardSquare square coord black) backgroundSquares

-- | Function to draw the board in its current state.
drawBoard :: Board -> Picture
drawBoard board = pictures $ boardBackground : map (\cell -> boardSquare square (position cell) (cellColor cell)) board

-- | Function to draw the following tetrominos that are in the side panel.
drawPanelTetros :: [Tetromino] -> Picture
drawPanelTetros tetros = pictures $ map drawOnePanelTetro (mvUpPanelTetro panelTetros)
  where
    drawOnePanelTetro t = pictures $ map (\position -> squareOutsideBoard square position (tcolor t)) (tcells t)
    panelTetros = map (sidePanelTetro . name) tetros
    mvUpPanelTetro [] = []
    mvUpPanelTetro (x : xs) = x : map (move upMv . move upMv . move upMv) (mvUpPanelTetro xs)

-- | Function to draw the side panel of the game.
drawSidePanel :: [Tetromino] -> Int -> Picture
drawSidePanel tetros _ = pictures [panelTetroBackground, panelTetroPic]
  where
    blackSqrCoords = [(x, y) | x <- [11 .. 16], y <- [1 .. 10]]
    panelTetroBackground = pictures $ map (\coord -> squareOutsideBoard square coord black) blackSqrCoords
    panelTetroPic = drawPanelTetros tetros 