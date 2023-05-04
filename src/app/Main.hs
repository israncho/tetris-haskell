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
import Graphics.Gloss.Interface.IO.Game (Event (EventKey), Key (Char, SpecialKey), SpecialKey (KeyEsc), playIO)
import System.Exit (exitSuccess)
import Tetromino (Name (I, O, T, J, L, S, Z), Tetromino, newTetromino)

data Game = Game {finished :: Bool, currTetro :: Tetromino}


-- | Display of the tetris.
tetrisDisplay :: Display
tetrisDisplay = InWindow "Tetris" (wWidth + 1, wHeight + 1) (200, 200)

drawBoard :: () -> IO Picture
drawBoard _ = return (translate (-halfWW) (-halfWH) (pictures [drawTetromino (newTetromino Z) False, grid]))

handleEvents :: Event -> () -> IO ()
handleEvents (EventKey (Char 'q') _ _ _) _ = exitSuccess
handleEvents (EventKey (SpecialKey KeyEsc) _ _ _) _ = exitSuccess
handleEvents _ _ = return ()

updateGame :: Float -> () -> IO ()
updateGame _ gameState = return gameState

main :: IO ()
main =
  playIO tetrisDisplay black 60 () drawBoard handleEvents updateGame
