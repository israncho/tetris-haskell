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
newTetromino Z = Tetromino S red [(4, 19), (5, 19), (5, 18), (6, 18)]

-- | Functions to move a position by one.
moveDown, moveLeft, moveRight :: Position -> Position
moveDown (x, y) = (x, y - 1)
moveLeft (x, y) = (x - 1, y)
moveRight (x, y) = (x + 1, y)

-- | Fuction to move a tetromino by one cell.
move :: (Position -> Position) -> Tetromino -> Tetromino
move movingFunct tetromino = tetromino {cells = map movingFunct (cells tetromino)}