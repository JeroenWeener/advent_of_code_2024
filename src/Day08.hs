module Day08 where

import Data.List (nub)

type C = (Int, Int)

solveDay08 :: IO ()
solveDay08 = do
  example <- readFile "input/day08_example.txt"
  input <- readFile "input/day08_input.txt"

  putStrLn "\n\nDay 08\n---\nPart 1"
  print $ part1 $ parseInput example
  print $ part1 $ parseInput input
  putStrLn "\nPart 2"
  print $ part2 $ parseInput example
  print $ part2 $ parseInput input

parseInput :: String -> ([[C]], Int)
parseInput input = (antennaCs, length ls)
  where
    ls = lines input
    antennaTypes = filter (`notElem` ".\n") . nub $ input
    antennaCs = [parseAntennaType c ls | c <- antennaTypes]

parseAntennaType :: Char -> [String] -> [C]
parseAntennaType c ls = [(y, x) | y <- [0 .. length ls - 1], x <- [0 .. length ls - 1], ls !! y !! x == c]

part1 :: ([[C]], Int) -> Int
part1 (css, dim) = length . filter (not . out dim) . nub $ concatMap antiNodes1 css

part2 :: ([[C]], Int) -> Int
part2 (css, dim) = length . nub $ concatMap (antiNodes2 dim) css

out :: Int -> C -> Bool
out dim (y, x) = y < 0 || y >= dim || x < 0 || x >= dim

antiNodes1 :: [C] -> [C]
antiNodes1 cs = concat [extrapolate1 c1 c2 | c1 <- cs, c2 <- cs, c1 /= c2]

extrapolate1 :: C -> C -> [C]
extrapolate1 (y1, x1) (y2, x2) = [c1, c2]
  where
    dy = y2 - y1
    dx = x2 - x1
    c1 = (y1 - dy, x1 - dx)
    c2 = (y2 + dy, x2 + dx)

antiNodes2 :: Int -> [C] -> [C]
antiNodes2 dim cs = concat [extrapolate2 c1 c2 dim | c1 <- cs, c2 <- cs, c1 /= c2]

extrapolate2 :: C -> C -> Int -> [C]
extrapolate2 c1@(y1, x1) (y2, x2) dim = plot startC dy dx dim
  where
    dy = y1 - y2
    dx = x1 - x2
    startC = first c1 dy dx dim

first :: C -> Int -> Int -> Int -> C
first c@(y, x) dy dx dim
  | out dim c' = c
  | otherwise = first c' dy dx dim
  where
    c' = (y - dy, x - dx)

plot :: C -> Int -> Int -> Int -> [C]
plot c@(y, x) dy dx dim
  | out dim c = []
  | otherwise = c : plot c' dy dx dim
  where
    c' = (y + dy, x + dx)
