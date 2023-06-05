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
downMv, leftMv, rightMv, upMv :: Position -> Position
downMv (x, y) = (x, y - 1)
leftMv (x, y) = (x - 1, y)
rightMv (x, y) = (x + 1, y)
upMv (x, y) = (x, y + 1)

-- | Function to rotate a position(point) around a pivot 90 degrees
rotation :: Position -> Position -> Position
rotation (x_ref, y_ref) (x, y) = (x'' + x_ref, y'' + y_ref)
  where
    (x', y') = (x - x_ref, y - y_ref)
    (x'', y'') = (-y', x')

-- | Returns the list of cells that make up the given row.
row :: Int -> Board -> [Cell]
row rowNum = filter (\cell -> snd (position cell) == rowNum)

-- | Given the row number, returns true if the row is complete.
isComplete :: Int -> Board -> Bool
isComplete x board = length (row x board) == 10

-- | Returns the list with the row numbers of the complete rows
completeRows :: Board -> [Int]
completeRows board = foldr (\row list -> if row `isComplete` board then row : list else list) [] [0 .. 19]

-- | Returns the highest complete row if there is any, in other case returns
-- the empty list
highestCompleteRow :: Board -> [Cell]
highestCompleteRow board = foldr (\row list -> if null list then checkComplete row else list) [] [0 .. 19]
  where
    checkComplete x = if x `isComplete` board then row x board else []

-- | Removes all the given cells from the board.
removeFrom :: [Cell] -> Board -> Board
removeFrom cellsTD = foldr (\cell list -> if cell `elem` cellsTD then list else cell : list) []

-- | Function to clear one row from a board given its row number and cells.
clearOneRow :: Int -> [Cell] -> Board -> Board
clearOneRow rowNum rowCells board = clearBoard
  where
    withoutRow = removeFrom rowCells board
    clearBoard =
      map
        ( \cell ->
            if snd (position cell) > rowNum
              then cell {position = downMv (position cell)}
              else cell
        )
        withoutRow

-- | Function to clear all complete rows from a board given its row numbers.
clearAllRows :: [Int] -> Board -> Board
clearAllRows [] board = board
clearAllRows rows board = clearAllRows (init rows) clearedBoard
  where
    rowToDelete = last rows
    rowCells = highestCompleteRow board
    sameRow = foldr (\cell -> (&&) (rowToDelete == snd (position cell))) True rowCells
    clearedBoard =
      if sameRow
        then clearOneRow rowToDelete rowCells board
        else error "cell not in the row to delete"