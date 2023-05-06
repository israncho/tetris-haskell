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
import Tetromino (Name (I, J, L, O, S, T, Z), Tetromino, move, moveTetromino, newTetromino)

data Game = Game
  { -- | Boolean to know if the game has ended
    finished :: Bool,
    -- | Falling tetromino.
    fTetro :: Tetromino,
    -- | Board of the game.
    board :: Board
  }
  deriving (Show, Eq)

initialStateGame = Game {finished = False, fTetro = newTetromino Z, board = []}

-- | Display of the tetris.
tetrisDisplay :: Display
tetrisDisplay = InWindow "Tetris" (wWidth + 1, wHeight + 1) (200, 200)

drawBoard :: Game -> IO Picture
drawBoard gameState = return (translate (-halfWW) (-halfWH) (pictures [drawTetromino (fTetro gameState) False, grid]))

handleEvents :: Event -> Game -> IO Game
handleEvents (EventKey (Char 'q') _ _ _) _ = exitSuccess
handleEvents (EventKey (SpecialKey KeyEsc) _ _ _) _ = exitSuccess
handleEvents (EventKey (Char 'j') Down _ _) game = return game {fTetro = moveTetromino moveLeft (fTetro game) (board game)}
handleEvents (EventKey (SpecialKey KeyLeft) Down _ _) game = return game {fTetro = moveTetromino moveLeft (fTetro game) (board game)}
handleEvents (EventKey (Char 'k') Down _ _) game = return game {fTetro = moveTetromino moveRight (fTetro game) (board game)}
handleEvents (EventKey (SpecialKey KeyRight) Down _ _) game = return game {fTetro = moveTetromino moveRight (fTetro game) (board game)}
handleEvents (EventKey (Char 'm') Down _ _) game = return game {fTetro = moveTetromino moveDown (fTetro game) (board game)}
handleEvents (EventKey (SpecialKey KeyDown) Down _ _) game = return game {fTetro = moveTetromino moveDown (fTetro game) (board game)}
handleEvents _ gameState = return gameState

updateGame :: Float -> Game -> IO Game
updateGame _ game = return game {fTetro = moveTetromino moveDown (fTetro game) (board game)}

main :: IO ()
main = playIO tetrisDisplay black 1 initialStateGame drawBoard handleEvents updateGame