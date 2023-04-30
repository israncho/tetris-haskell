module Main where

import Board
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (Event, playIO)

-- | Constants for the display
wWidth, wHeight :: Int
wWidth = 300
wHeight = 600

-- | Constants for the display
halfWW, halfWH :: Float
halfWW = fromIntegral wWidth / 2
halfWH = fromIntegral wHeight / 2

-- | Display of the tetris.
tetrisDisplay :: Display
tetrisDisplay = InWindow "Tetris" (wWidth + 1, wHeight + 1) (200, 200)

drawBoard :: () -> IO Picture
drawBoard _ = return (translate (-halfWW) (-halfWH) (pictures [square (30, 30) red, grid]))

handleEvents :: Event -> () -> IO ()
handleEvents _ _ = return ()

updateGame :: Float -> () -> IO ()
updateGame _ gameState = return gameState

main :: IO ()
main =
  playIO tetrisDisplay black 60 () drawBoard handleEvents updateGame
