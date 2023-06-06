-- | Module for tetrominos
module Tetromino where

import Board
import Graphics.Gloss
import System.Random

-- | Names for tetrominos
data Name = I | O | T | J | L | S | Z
  deriving (Eq, Show)

names :: [Name]
names = [I, O, T, J, L, S, Z]

-- | Type to represent tetrominos
data Tetromino = Tetromino
  { -- | The name of the tetromino
    name :: Name,
    -- | The color of the tetromino
    tcolor :: Color,
    -- | The positions of the cells that make up the tetromino
    tcells :: [Position]
  }
  deriving (Eq, Show)

-- | Returns a tetromino in the initial position.
newTetromino :: Name -> Tetromino
newTetromino I = Tetromino I cyan [(4, 19), (3, 19), (5, 19), (6, 19)]
newTetromino O = Tetromino O yellow [(4, 19), (4, 18), (5, 19), (5, 18)]
newTetromino T = Tetromino T rose [(5, 19), (4, 19), (6, 19), (5, 18)]
newTetromino J = Tetromino J (makeColor 0.2 0.2 0.9 1.0) [(5, 19), (4, 19), (6, 19), (6, 18)]
newTetromino L = Tetromino L orange [(4, 19), (3, 19), (5, 19), (3, 18)]
newTetromino S = Tetromino S green [(5, 19), (4, 18), (5, 18), (6, 19)]
newTetromino Z = Tetromino Z red [(5, 19), (4, 19), (5, 18), (6, 18)]

-- | Fuction to move all the cells of the tetromino by one.
move :: (Position -> Position) -> Tetromino -> Tetromino
move movingFunct tetromino = tetromino {tcells = map movingFunct (tcells tetromino)}

-- | Returns true if there is a collision on the board.
-- This occurs when a tetromino shares a position with an already occupied cell on the board.
collision :: Tetromino -> Board -> Bool
collision tetro board = foldr ((||) . (`elem` boardPositions)) False (tcells tetro)
  where
    boardPositions = map position board

-- | Returns true if all the cells of the tetromino are in bounds.
allInBounds :: Tetromino -> Bool
allInBounds tetro = foldr ((&&) . inBounds) True (tcells tetro)

-- | Returns true if the given tetromino can move in the given direction within the board.
canMove :: (Position -> Position) -> Tetromino -> Board -> Bool
canMove movingFunct tetro board = allInBounds movedTetro && not (collision movedTetro board)
  where
    movedTetro = move movingFunct tetro

-- | Returns a moved tetromino if it's possible to move.
moveTetromino :: (Position -> Position) -> Tetromino -> Board -> Tetromino
moveTetromino movingFun tetro board
  | canMove movingFun tetro board = move movingFun tetro
  | otherwise = tetro

-- | Function to move a tetromino all the way in a given direction.
moveAllTheWay :: (Position -> Position) -> Tetromino -> Board -> Tetromino
moveAllTheWay movingFun tetro board
  | not $ canMove movingFun tetro board = tetro
  | otherwise = moveAllTheWay movingFun (move movingFun tetro) board

-- | Returns the cells of the tetromino.
getCells :: Tetromino -> [Cell]
getCells tetro = map (\x -> Cell {position = x, cellColor = tcolor tetro}) $ tcells tetro

-- | Returns a random tetromino in it's initial position.
randomTetro :: IO Tetromino
randomTetro = do
  index <- randomRIO (0, 6)
  return $ newTetromino (names !! index)

-- | Function to rotate all the cells of a tetromino around its center.
rotateTCells :: Tetromino -> Tetromino
rotateTCells (Tetromino {name = O, tcells = ocells}) = Tetromino O yellow ocells
rotateTCells tetro = tetro {tcells = rotatedcells}
  where
    pivot = head $ tcells tetro
    rotatedcells = pivot : map (rotation pivot) (tail $ tcells tetro)

-- | Returns true if the given tetro can rotate.
canRotate :: Tetromino -> Board -> Bool
canRotate tetro board = allInBounds rotatedTetro && not (collision rotatedTetro board)
  where
    rotatedTetro = rotateTCells tetro

-- | Function to rotate a tetromino in the given board.
rotateTetro :: Tetromino -> Board -> Tetromino
rotateTetro tetro board
  | canRotate tetro board = rotateTCells tetro
  | canRotate oneRight board = rotateTCells oneRight -- wallkicks start here
  | canRotate oneLeft board = rotateTCells oneLeft
  | canRotate twoRight board = rotateTCells twoRight
  | canRotate twoLeft board = rotateTCells twoLeft
  | canRotate oneDown board = rotateTCells oneDown
  | canRotate twoDown board = rotateTCells twoDown
  | otherwise = tetro
  where
    oneRight = move rightMv tetro
    twoRight = move rightMv oneRight
    oneLeft = move leftMv tetro
    twoLeft = move leftMv oneLeft
    oneDown = move downMv tetro
    twoDown = move downMv oneDown 

-- | Returns a tetromino in the bottom position of the side panel
sidePanelTetro :: Name -> Tetromino
sidePanelTetro I = Tetromino I cyan [(12, 2), (13, 2), (14, 2), (15, 2)]
sidePanelTetro O = Tetromino O yellow [(13, 3), (13, 2), (14, 2), (14, 3)]
sidePanelTetro T = Tetromino T rose [(13, 3), (14, 3), (15, 3), (14, 2)]
sidePanelTetro J = Tetromino J (makeColor 0.2 0.2 0.9 1.0) [(13, 3), (14, 3), (15, 3), (15, 2)]
sidePanelTetro L = Tetromino L orange [(13, 3), (14, 3), (15, 3), (13, 2)]
sidePanelTetro S = Tetromino S green [(13, 2), (14, 2), (14, 3), (15, 3)]
sidePanelTetro Z = Tetromino Z red [(13, 3), (14, 3), (14, 2), (15, 2)]