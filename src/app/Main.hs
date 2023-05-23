module Main where

import Board (Board, downMv, leftMv, rightMv)
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
    SpecialKey (KeyDown, KeyEsc, KeyLeft, KeyRight),
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

initialStateGame = Game {finished = False, fTetro = newTetromino Z, board = [], nTetros = firstTetros}

-- | Display of the tetris.
tetrisDisplay :: Display
tetrisDisplay = InWindow "Tetris" (wWidth + 1, wHeight + 1) (200, 200)

drawGame :: Game -> IO Picture
drawGame gameState =
  return (translate (-halfWW) (-halfWH) (pictures [boardpic, ftetropic, ghostTetro, grid]))
  where
    currBoard = board gameState
    currTetro = fTetro gameState
    boardpic = drawBoard currBoard
    ftetropic = drawTetromino currTetro False
    ghostTetro = drawTetromino (moveAllTheWay downMv currTetro currBoard) True

handleEvents :: Event -> Game -> IO Game
handleEvents (EventKey (Char 'q') _ _ _) _ = exitSuccess
handleEvents (EventKey (Char 'j') Down _ _) game = return game {fTetro = moveTetromino leftMv (fTetro game) (board game)}
handleEvents (EventKey (Char 'k') Down _ _) game = return game {fTetro = moveTetromino rightMv (fTetro game) (board game)}
handleEvents (EventKey (Char 'm') Down _ _) game = return game {fTetro = moveTetromino downMv (fTetro game) (board game)}
handleEvents _ gameState = return gameState

updateGame :: Float -> Game -> IO Game
updateGame _ game
  | canMove downMv currTetro currBoard = return game {fTetro = moveTetromino downMv currTetro currBoard}
  | not $ collision (last nextTetros) currBoard = do
      rndTetro <- randomTetro
      return
        game {board = getCells currTetro ++ currBoard, fTetro = last nextTetros, nTetros = rndTetro : init nextTetros}
  | otherwise = exitSuccess
  where
    currTetro = fTetro game
    currBoard = board game
    nextTetros = nTetros game

main :: IO ()
main = playIO tetrisDisplay black 1 initialStateGame drawGame handleEvents updateGame