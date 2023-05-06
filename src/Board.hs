module Board where

import Graphics.Gloss (Color)

-- | Position in the board
type Position = (Int, Int)

-- | Data type for the cells in the board.
data Cell = Cell {position :: Position, cellColor :: Color} deriving (Show, Eq)

-- | Data type for the board.
type Board = [Cell]

-- | Returns true if a position is within the bounds of the board.
inBounds :: Position -> Bool
inBounds (x, y)
  | x < 0 || x > 9 = False
  | y < 0 || y > 19 = False
  | otherwise = True

-- | Functions to move a position by one.
moveDown, moveLeft, moveRight :: Position -> Position
moveDown (x, y) = (x, y - 1)
moveLeft (x, y) = (x - 1, y)
moveRight (x, y) = (x + 1, y)