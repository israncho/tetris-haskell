module Main where

import Board (Board, Position, clearAllRows, completeRows, downMv, leftMv, rightMv)
import Control.Monad (replicateM)
import Drawing
import Graphics.Gloss
  ( Display (InWindow),
    Picture,
    black,
    pictures,
    translate,
  )
import Graphics.Gloss.Interface.IO.Game
  ( Event (EventKey),
    Key (Char, SpecialKey),
    KeyState (Down, Up),
    SpecialKey (KeyDown, KeyLeft, KeyRight, KeySpace, KeyUp),
    playIO,
  )
import System.Exit (exitSuccess)
import Tetromino
  ( Name (I, J, L, O, S, T, Z),
    Tetromino,
    canMove,
    collision,
    getCells,
    moveAllTheWay,
    moveTetromino,
    newTetromino,
    randomTetro,
    rotateTetro,
  )

data Game = Game
  { -- | Falling tetromino.
    fTetro :: Tetromino,
    -- | Next tetrominos
    nTetros :: [Tetromino],
    -- | Board of the game.
    board :: Board,
    -- | Score, number of cleared rows
    score :: Int,
    -- | Updates made
    updateCount :: Int,
    -- | iteration to take action.
    itAction :: Int
  }
  deriving (Show, Eq)

-- | Display of the tetris.
tetrisDisplay :: Display
tetrisDisplay = InWindow "Tetris" (wWidth + 1, wHeight + 1) (200, 200)

-- | Function to draw the entire game.
drawGame :: Game -> IO Picture
drawGame gameState =
  return (translate (-halfWW) (-halfWH) (pictures [boardpic, ftetropic, ghostTetro, grid, sidePanelPic]))
  where
    currBoard = board gameState
    currTetro = fTetro gameState
    boardpic = drawBoard currBoard
    sidePanelPic = drawSidePanel (nTetros gameState) (score gameState)
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

clearRows :: Game -> Game
clearRows game = game {board = clearAllRows cRows currBoard, score = newScore}
  where
    currBoard = board game
    cRows = completeRows currBoard
    newScore = score game + length cRows

-- | Function to handle the inputs(events) of the user.
handleEvents :: Event -> Game -> IO Game
handleEvents (EventKey (Char 'q') _ _ _) game = exitSuccess
handleEvents (EventKey (Char 'j') Down _ _) game = performOneMove leftMv game
handleEvents (EventKey (Char 'l') Down _ _) game = performOneMove rightMv game
handleEvents (EventKey (Char 'k') Down _ _) game = performOneMove downMv game
handleEvents (EventKey (SpecialKey KeyLeft) Down _ _) game = performOneMove leftMv game
handleEvents (EventKey (SpecialKey KeyRight) Down _ _) game = performOneMove rightMv game
handleEvents (EventKey (SpecialKey KeyDown) Down _ _) game = performOneMove downMv game
handleEvents (EventKey (SpecialKey KeySpace) Down _ _) game = do
  currGame <- lockAndSpawnTetromino game {fTetro = moveAllTheWay downMv (fTetro game) (board game)}
  return $ clearRows currGame
handleEvents (EventKey (Char 'i') Down _ _) game = return game {fTetro = rotateTetro (fTetro game) (board game)}
handleEvents (EventKey (SpecialKey KeyUp) Down _ _) game = return game {fTetro = rotateTetro (fTetro game) (board game)}
handleEvents _ gameState = return gameState

mvTetroFalling :: Game -> Game
mvTetroFalling game 
  | updateCount game == itAction game = game {fTetro = movedTetro, updateCount = 0}
  | otherwise = game {updateCount = ucount + 1}
  where
    ucount = updateCount game
    itActi = itAction game
    currTetro = fTetro game
    currBoard = board game
    movedTetro = moveTetromino downMv currTetro currBoard

-- | Function to update the game and step the game one iteration.
updateGame :: Float -> Game -> IO Game
updateGame _ game
  | canMove downMv currTetro currBoard = return $ mvTetroFalling game 
  | collision nextTetro currBoard || not (canMove downMv nextTetro currBoard) = exitSuccess
  | otherwise = do
      currGame <- lockAndSpawnTetromino game
      return $ clearRows currGame
  where
    currTetro = fTetro game
    currBoard = board game
    nextTetro = last (nTetros game)

-- | The main entry point of the Tetris game. It initializes the game state and starts the game loop.
main :: IO ()
main = do
  firstTetro <- randomTetro
  nextTetros <- replicateM 3 randomTetro
  let initialStateGame =
        Game
          { fTetro = firstTetro,
            board = [],
            nTetros = nextTetros,
            score = 0,
            updateCount = 0,
            itAction = 30
          }
  playIO tetrisDisplay grey 30 initialStateGame drawGame handleEvents updateGame