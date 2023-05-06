module Main where

import Drawing
import Graphics.Gloss
  ( Display (InWindow),
    Picture,
    black,
    pictures,
    red,
    translate,
  )
import Graphics.Gloss.Interface.IO.Game (Event (EventKey), Key (Char, SpecialKey), KeyState (Down, Up), SpecialKey (KeyEsc, KeyLeft, KeyRight, KeyDown), playIO)
import System.Exit (exitSuccess)
import Tetromino (Name (I, J, L, O, S, T, Z), Tetromino, move, moveLeft, moveRight, newTetromino, moveDown)

data Game = Game {finished :: Bool, currTetro :: Tetromino} deriving (Show, Eq)

initialStateGame = Game {finished = False, currTetro = newTetromino Z}

-- | Display of the tetris.
tetrisDisplay :: Display
tetrisDisplay = InWindow "Tetris" (wWidth + 1, wHeight + 1) (200, 200)

drawBoard :: Game -> IO Picture
drawBoard gameState = return (translate (-halfWW) (-halfWH) (pictures [drawTetromino (currTetro gameState) False, grid]))

handleEvents :: Event -> Game -> IO Game
handleEvents (EventKey (Char 'q') _ _ _) _ = exitSuccess
handleEvents (EventKey (SpecialKey KeyEsc) _ _ _) _ = exitSuccess
handleEvents (EventKey (Char 'j') Down _ _) game = return game {currTetro = move moveLeft $ currTetro game}
handleEvents (EventKey (SpecialKey KeyLeft) Down _ _) game = return game {currTetro = move moveLeft $ currTetro game}
handleEvents (EventKey (Char 'k') Down _ _) game = return game {currTetro = move moveRight $ currTetro game}
handleEvents (EventKey (SpecialKey KeyRight) Down _ _) game = return game {currTetro = move moveRight $ currTetro game}
handleEvents (EventKey (Char 'm') Down _ _) game = return game {currTetro = move moveDown $ currTetro game}
handleEvents (EventKey (SpecialKey KeyDown) Down _ _) game = return game {currTetro = move moveDown $ currTetro game}
handleEvents _ gameState = return gameState

updateGame :: Float -> Game -> IO Game
updateGame _ game = return game {currTetro = move moveDown $ currTetro game}

main :: IO ()
main = playIO tetrisDisplay black 1 initialStateGame drawBoard handleEvents updateGame