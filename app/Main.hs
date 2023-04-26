module Main where

import Graphics.Gloss

main :: IO ()
main = display (InWindow "Mi ventana" (640, 480) (0, 0)) white drawingFunction

drawingFunction :: Picture
drawingFunction = translate (-50) 50 $ color red $ circle 80

