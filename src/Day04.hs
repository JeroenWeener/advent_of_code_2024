module Day04 where

import Data.List (transpose)

solveDay04 :: IO ()
solveDay04 = do
  example <- readFile "input/day04_example.txt"
  input <- readFile "input/day04_input.txt"

  putStrLn "\n\nDay 04\n---\nPart 1"
  print $ part1 $ parseInput example
  print $ part1 $ parseInput input
  putStrLn "\nPart 2"
  print $ part2 $ parseInput example
  print $ part2 $ parseInput input

parseInput :: String -> [String]
parseInput = lines

part1 :: [String] -> Int
part1 input = h + v + d + a
  where
    h = sum $ map count input
    v = sum $ map count $ transpose input
    d = sum $ map count $ diagonals input
    a = sum $ map count $ diagonals $ reverse input

part2 :: [String] -> Int
part2 input = length . filter id $ map (isX input) (centers input)

count :: String -> Int
count ('X' : 'M' : 'A' : 'S' : ts) = 1 + count ('S' : ts)
count ('S' : 'A' : 'M' : 'X' : ts) = 1 + count ('X' : ts)
count (_ : ts) = count ts
count _ = 0

diagonals :: [String] -> [String]
diagonals ts =
  [[ts !! (x + y) !! y | y <- [0 .. length ts - x - 1]] | x <- [0 .. length ts - 4]]
    ++ [[ts !! y !! (x + y) | y <- [0 .. length ts - x - 1]] | x <- [1 .. length ts - 4]]

centers :: [String] -> [(Int, Int)]
centers ts = [(x, y) | x <- [1 .. length ts - 2], y <- [1 .. length ts - 2], ts !! x !! y == 'A']

isX :: [String] -> (Int, Int) -> Bool
isX ts (x, y) = (d == "MS" || d == "SM") && (a == "MS" || a == "SM")
  where
    ul = ts !! (x - 1) !! (y - 1)
    ur = ts !! (x + 1) !! (y - 1)
    ll = ts !! (x - 1) !! (y + 1)
    lr = ts !! (x + 1) !! (y + 1)
    d = [ul, lr]
    a = [ll, ur]
