-- | Module for tetrominos
module Tetromino where

import Graphics.Gloss

-- | Position in the board
type Position = (Int, Int)

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
newTetromino :: Name -> Color -> Tetromino
newTetromino I tcolor = Tetromino I tcolor [(4, 19), (4, 18), (4, 17), (4, 16)]
