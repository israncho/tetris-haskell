module Board where

import Graphics.Gloss (Color)

-- | Position in the board
type Position = (Int, Int)

-- | Data type for the cells in the board.
data Cell = Cell {position :: Position, cellColor :: Color}

-- | Data type for the board.
type Board = [Cell]