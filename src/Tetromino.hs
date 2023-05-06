-- | Module for tetrominos
module Tetromino where

import Board
import Graphics.Gloss

-- | Names for tetrominos
data Name = I | O | T | J | L | S | Z
  deriving (Eq, Show)

-- | Type to represent tetrominos
data Tetromino = Tetromino
  { -- | The name of the tetromino
    name :: Name,
    -- | The color of the tetromino
    tcolor :: Color,
    -- | The positions of the cells that make up the tetromino
    cells :: [Position]
  }
  deriving (Eq, Show)

-- | Returns a tetromino in the initial position.
newTetromino :: Name -> Tetromino
newTetromino I = Tetromino I cyan [(3, 19), (4, 19), (5, 19), (6, 19)]
newTetromino O = Tetromino O yellow [(4, 19), (4, 18), (5, 19), (5, 18)]
newTetromino T = Tetromino T rose [(4, 19), (5, 19), (6, 19), (5, 18)]
newTetromino J = Tetromino J blue [(3, 19), (4, 19), (5, 19), (5, 18)]
newTetromino L = Tetromino L orange [(3, 19), (4, 19), (5, 19), (3, 18)]
newTetromino S = Tetromino S green [(4, 18), (5, 18), (5, 19), (6, 19)]
newTetromino Z = Tetromino Z red [(4, 19), (5, 19), (5, 18), (6, 18)]

-- | Fuction to move all the cells of the tetromino by one.
move :: (Position -> Position) -> Tetromino -> Tetromino
move movingFunct tetromino = tetromino {cells = map movingFunct (cells tetromino)}

-- | Returns true if the given tetromino can move in the given direction within the board.
canMove :: (Position -> Position) -> Tetromino -> Board -> Bool
canMove movingFunct tetro board = allInBounds && not collision
  where
    movedTetro = move movingFunct tetro
    allInBounds = foldr ((&&) . inBounds) True (cells movedTetro)
    boardPositions = map position board
    collision = foldr ((||) . (`elem` boardPositions)) False (cells movedTetro)

-- | Returns a moved tetromino if it's possible to move.
moveTetromino :: (Position -> Position) -> Tetromino -> Board -> Tetromino
moveTetromino movingFun tetro board
  | canMove movingFun tetro board = move movingFun tetro
  | otherwise = tetro
