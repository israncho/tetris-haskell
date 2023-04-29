module Board where

myZipp :: [(Int, Int)] -> [(Int, Int)] -> [[(Float, Float)]]
myZipp [] _ = []
myZipp ((a, b) : xs) ((c, d) : ys) = [(iToF a, iToF b), (iToF c, iToF d)] : myZipp xs ys

iToF :: Int -> Float
iToF n = fromIntegral n :: Float

basePoints = [(x * 30, 0) | x <- [0 .. 10], x * 30 `mod` 30 == 0]

ceilPoints = [(x * 30, 600) | x <- [0 .. 10], x * 30 `mod` 30 == 0]

vLines = myZipp basePoints ceilPoints

leftPoints = [(0, x*30) | x <- [0..20], x*30 `mod` 30 == 0]

rightPoints = [(300, x*30) | x <- [0..20], x*30 `mod` 30 == 0]

hLines = myZipp leftPoints rightPoints
