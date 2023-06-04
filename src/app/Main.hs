module Main where

import Board (Board, Position, completeRows, downMv, highestCompleteRow, leftMv, rightMv)
import Drawing
import Graphics.Gloss
  ( Display (InWindow),
    Picture,
    black,
    pictures,
    red,
    translate,
  )
import Graphics.Gloss.Interface.IO.Game
  ( Event (EventKey),
    Key (Char, SpecialKey),
    KeyState (Down, Up),
    SpecialKey (KeyDown, KeyEsc, KeyLeft, KeyRight, KeySpace),
    playIO,
  )
import System.Exit (exitSuccess)
import Tetromino
  ( Name (I, J, L, O, S, T, Z),
    Tetromino,
    canMove,
    collision,
    getCells,
    move,
    moveAllTheWay,
    moveTetromino,
    newTetromino,
    randomTetro,
    rotateTetro,
  )

data Game = Game
  { -- | Boolean to know if the game has ended
    finished :: Bool,
    -- | Falling tetromino.
    fTetro :: Tetromino,
    -- | Next tetrominos
    nTetros :: [Tetromino],
    -- | Board of the game.
    board :: Board
  }
  deriving (Show, Eq)

firstTetros = [newTetromino I, newTetromino O, newTetromino L]

initialStateGame = Game {finished = False, fTetro = newTetromino I, board = [], nTetros = firstTetros}

-- | Display of the tetris.
tetrisDisplay :: Display
tetrisDisplay = InWindow "Tetris" (wWidth + 1, wHeight + 1) (200, 200)

-- | Function to draw the entire game.
drawGame :: Game -> IO Picture
drawGame gameState =
  return (translate (-halfWW) (-halfWH) (pictures [boardpic, ftetropic, ghostTetro, grid]))
  where
    currBoard = board gameState
    currTetro = fTetro gameState
    boardpic = drawBoard currBoard
    ftetropic = drawTetromino currTetro False
    ghostTetro = drawTetromino (moveAllTheWay downMv currTetro currBoard) True

-- | Function to make the falling tetromino move.
performOneMove :: (Position -> Position) -> Game -> IO Game
performOneMove direction game = return game {fTetro = moveTetromino direction (fTetro game) (board game)}

-- | Locks the current tetromino and spawns another one.
lockAndSpawnTetromino :: Game -> IO Game
lockAndSpawnTetromino game = do
  rndTetro <- randomTetro
  return game {board = getCells currTetro ++ currBoard, fTetro = last nextTetros, nTetros = rndTetro : init nextTetros}
  where
    currTetro = fTetro game
    currBoard = board game
    nextTetros = nTetros game

-- | Function to handle the inputs(events) of the user.
handleEvents :: Event -> Game -> IO Game
handleEvents (EventKey (Char 'q') _ _ _) game = do
  print (highestCompleteRow $ board game)
  print (completeRows $ board game)
  exitSuccess
handleEvents (EventKey (Char 'j') Down _ _) game = performOneMove leftMv game
handleEvents (EventKey (Char 'l') Down _ _) game = performOneMove rightMv game
handleEvents (EventKey (Char 'k') Down _ _) game = performOneMove downMv game
handleEvents (EventKey (SpecialKey KeyLeft) Down _ _) game = performOneMove leftMv game
handleEvents (EventKey (SpecialKey KeyRight) Down _ _) game = performOneMove rightMv game
handleEvents (EventKey (SpecialKey KeyDown) Down _ _) game = performOneMove downMv game
handleEvents (EventKey (SpecialKey KeySpace) Down _ _) game =
  lockAndSpawnTetromino game {fTetro = moveAllTheWay downMv (fTetro game) (board game)}
handleEvents (EventKey (Char 'i') Down _ _) game = return game {fTetro = rotateTetro (fTetro game) (board game)}
handleEvents _ gameState = return gameState

-- | Function to update the game and step the game one iteration.
updateGame :: Float -> Game -> IO Game
updateGame _ game
  | canMove downMv currTetro currBoard = return game {fTetro = moveTetromino downMv currTetro currBoard}
  | not $ collision (last nextTetros) currBoard = lockAndSpawnTetromino game
  | otherwise = exitSuccess
  where
    currTetro = fTetro game
    currBoard = board game
    nextTetros = nTetros game

-- | The main entry point of the Tetris game. It initializes the game state and starts the game loop.
main :: IO ()
main = playIO tetrisDisplay black 1 initialStateGame drawGame handleEvents updateGame