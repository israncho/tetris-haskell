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
newTetromino I = Tetromino I cyan [(3, 19), (4, 19), (5, 19), (6, 19)]
newTetromino O = Tetromino O yellow [(4, 19), (4, 18), (5, 19), (5, 18)]
newTetromino T = Tetromino T rose [(5, 19), (4, 19), (6, 19), (5, 18)]
newTetromino J = Tetromino J blue [(5, 19), (4, 19), (6, 19), (6, 18)]
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

-- | Function to rotate a tetromino.
rotate :: Tetromino -> Tetromino
rotate (Tetromino {name = I, tcells = ((x, y) : xs)})
  | y == snd (head xs) = Tetromino I cyan [(x + 1, y + 1), (x + 1, y), (x + 1, y - 1), (x + 1, y - 2)]
  | otherwise = Tetromino I cyan [(x - 1, y - 1), (x, y - 1), (x + 1, y - 1), (x + 2, y - 1)]
rotate (Tetromino {name = O, tcells = ocells}) = Tetromino O yellow ocells
rotate tetro = tetro {tcells = rotatedcells}
  where
    pivot = head $ tcells tetro
    rotatedcells = pivot : map (rotation pivot) (tail $ tcells tetro)
