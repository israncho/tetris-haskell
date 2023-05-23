module Main where

import Board (Board, moveDown, moveLeft, moveRight)
import Drawing
import Graphics.Gloss
  ( Display (InWindow),
    Picture,
    black,
    pictures,
    red,
    translate,
  )
import Graphics.Gloss.Interface.IO.Game (Event (EventKey), Key (Char, SpecialKey), KeyState (Down, Up), SpecialKey (KeyDown, KeyEsc, KeyLeft, KeyRight), playIO)
import System.Exit (exitSuccess)
import Tetromino (Name (I, J, L, O, S, T, Z), Tetromino, canMove, collision, getCells, move, moveTetromino, newTetromino, randomTetro)

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
  return (translate (-halfWW) (-halfWH) (pictures [boardpic, ftetropic, grid]))
  where
    boardpic = drawBoard $ board gameState
    ftetropic = drawTetromino (fTetro gameState) False

handleEvents :: Event -> Game -> IO Game
handleEvents (EventKey (Char 'q') _ _ _) _ = exitSuccess
handleEvents (EventKey (Char 'j') Down _ _) game = return game {fTetro = moveTetromino moveLeft (fTetro game) (board game)}
handleEvents (EventKey (Char 'k') Down _ _) game = return game {fTetro = moveTetromino moveRight (fTetro game) (board game)}
handleEvents (EventKey (Char 'm') Down _ _) game = return game {fTetro = moveTetromino moveDown (fTetro game) (board game)}
handleEvents _ gameState = return gameState

updateGame :: Float -> Game -> IO Game
updateGame _ game
  | canMove moveDown currTetro currBoard = return game {fTetro = moveTetromino moveDown currTetro currBoard}
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