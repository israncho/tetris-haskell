-- | Module for tetrominos
module Tetromino where

-- | Colors for tetrominos
data Color = Red | Green | Cyan | Yellow | Orange | Blue
  deriving (Eq, Show)

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
