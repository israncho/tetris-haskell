module Main where

import System.IO

import Board
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (playIO, Event)


wWidth, wHeight, cellSize :: Int

-- | window width
wWidth = 300

-- | window height
wHeight = 600

-- | cell size
cellSize = 30

halfWW, halfWH :: Float

-- | half of the width
halfWW = fromIntegral wWidth / 2

-- | half of the height
halfWH = fromIntegral wHeight / 2


-- | Display of the tetris.
tetrisDisplay :: Display
tetrisDisplay = InWindow "Tetris" (wWidth + 1, wHeight + 1) (200, 200)


drawBoard :: () -> IO Picture
drawBoard _ =  return (translate (-halfWW) (-halfWH) (pictures [square (30, 30) red, grid]))

handleEvents :: Event -> () -> IO ()
handleEvents _ _ = return ()

updateGame :: Float -> () -> IO ()
updateGame _ gameState = return gameState

main :: IO ()
main =
  playIO tetrisDisplay black 60 () drawBoard handleEvents updateGame 

