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
    -- | Upcoming tetrominos.
    upcomingTetros :: [Tetromino],
    -- | Board of the game.
    board :: Board,
    -- | Score, number of cleared rows.
    score :: Int,
    -- | Updates made in one iteration of the game.
    updateCount :: Int,
    -- | Number of updates needed to iterate the game.
    updatesToIterate :: Int
  }
  deriving (Show, Eq)

-- | Display of the tetris.
tetrisDisplay :: Display
tetrisDisplay = InWindow "Tetris" (wWidth + 1, wHeight + 1) (200, 200)

-- | Function to draw the entire game.
drawGame :: Game -> IO Picture
drawGame gameState =
  return (translate (-halfWW) (-halfWH) gamePic)
  where
    currBoard = board gameState
    currTetro = fTetro gameState
    boardPic = drawBoard currBoard
    fTetroPic = drawTetromino currTetro False
    gTetroPic = drawTetromino (moveAllTheWay downMv currTetro currBoard) True
    sidePanel = drawSidePanel (upcomingTetros gameState) (score gameState)
    gamePic = pictures [boardPic, fTetroPic, gTetroPic, grid, sidePanel]

-- | Function to make the falling tetromino move.
performOneMove :: (Position -> Position) -> Game -> IO Game
performOneMove direction game = return game {fTetro = movedTetro}
  where
    movedTetro = moveTetromino direction (fTetro game) (board game)

-- | Function to make the falling tetromino rotate.
performRotation :: Game -> IO Game
performRotation game = return game {fTetro = rotateTetro (fTetro game) (board game)}

-- | Locks the current tetromino and spawns another one.
lockSpawnTetro :: Game -> IO Game
lockSpawnTetro game = do
  rndTetro <- randomTetro
  return game {board = newBoard, fTetro = newFallingTetro, upcomingTetros = rndTetro : rTetros}
  where
    currTetro = fTetro game
    currBoard = board game
    uTetros = upcomingTetros game
    newBoard = getCells currTetro ++ currBoard
    newFallingTetro = last uTetros
    rTetros = init uTetros

-- | Clears all the complete rows in the game and
-- updates the score.
clearRows :: Game -> Game
clearRows game = game {board = clearAllRows cRows currBoard, score = newScore}
  where
    currBoard = board game
    cRows = completeRows currBoard
    newScore = score game + length cRows

-- | Function to handle the inputs(events) of the user.
handleEvents :: Event -> Game -> IO Game
handleEvents (EventKey (Char 'q') _ _ _) game = do
  putStrLn ("\nScore: " ++ show (score game))
  exitSuccess
handleEvents (EventKey (Char 'i') Down _ _) game = performRotation game
handleEvents (EventKey (Char 'j') Down _ _) game = performOneMove leftMv game
handleEvents (EventKey (Char 'l') Down _ _) game = performOneMove rightMv game
handleEvents (EventKey (Char 'k') Down _ _) game = performOneMove downMv game
handleEvents (EventKey (SpecialKey KeyUp) Down _ _) game = performRotation game
handleEvents (EventKey (SpecialKey KeyLeft) Down _ _) game = performOneMove leftMv game
handleEvents (EventKey (SpecialKey KeyRight) Down _ _) game = performOneMove rightMv game
handleEvents (EventKey (SpecialKey KeyDown) Down _ _) game = performOneMove downMv game
handleEvents (EventKey (SpecialKey KeySpace) Down _ _) game = do
  currGame <- lockSpawnTetro game {fTetro = moveAllTheWay downMv (fTetro game) (board game)}
  return $ (clearRows currGame) {updateCount = 0}
handleEvents _ gameState = return gameState

-- | Function move down the falling tetromino.
-- Returns the game with the tetromino falling one
-- position down if it is time to iterate the game
mvTetroFalling :: Game -> IO Game
mvTetroFalling game
  | canIterate = performOneMove downMv (game {updateCount = 0})
  | otherwise = return game {updateCount = updatecnt + 1}
  where
    updatecnt = updateCount game
    canIterate = updatecnt >= updatesToIterate game

-- | Function to update the game and step the game one iteration.
updateGame :: Float -> Game -> IO Game
updateGame _ game
  | canMove downMv currTetro currBoard = mvTetroFalling game
  | collision nextTetro currBoard = do
      putStrLn ("\nScore: " ++ show (score game))
      exitSuccess
  | canIterate = do
      currGame <- lockSpawnTetro game
      return $ (clearRows currGame) {updateCount = 0}
  | otherwise = return game {updateCount = updatecnt + 1}
  where
    currTetro = fTetro game
    currBoard = board game
    nextTetro = last (upcomingTetros game)
    updatecnt = updateCount game
    canIterate = updatecnt >= updatesToIterate game

-- | The main entry point of the Tetris game. 
-- It initializes the game state and starts the game loop.
main :: IO ()
main = do
  firstTetro <- randomTetro
  nextTetros <- replicateM 3 randomTetro
  let initialStateGame =
        Game
          { fTetro = firstTetro,
            board = [],
            upcomingTetros = nextTetros,
            score = 0,
            updateCount = 0,
            updatesToIterate = 30
          }
  playIO tetrisDisplay grey 30 initialStateGame drawGame handleEvents updateGame