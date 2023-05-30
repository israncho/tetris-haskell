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
downMv, leftMv, rightMv :: Position -> Position
downMv (x, y) = (x, y - 1)
leftMv (x, y) = (x - 1, y)
rightMv (x, y) = (x + 1, y)

-- | Function to rotate a position(point) around a pivot 90 degrees
rotation :: Position -> Position -> Position
rotation (x_ref, y_ref) (x, y) = (x'' + x_ref, y'' + y_ref)
  where
    (x', y') = (x - x_ref, y - y_ref)
    (x'', y'') = (-y', x')