module Day11 where

import Data.MemoTrie (memo2)

solveDay11 :: IO ()
solveDay11 = do
  example <- readFile "input/day11_example.txt"
  input <- readFile "input/day11_input.txt"

  putStrLn "\n\nDay 11\n---\nPart 1"
  print $ part1 $ parseInput example
  print $ part1 $ parseInput input
  putStrLn "\nPart 2"
  print $ part2 $ parseInput example
  print $ part2 $ parseInput input

parseInput :: String -> [Int]
parseInput = map read . words

part1 :: [Int] -> Int
part1 = sum . map (flutter 25)

part2 :: [Int] -> Int
part2 = sum . map (flutter 75)

blink :: Int -> [Int]
blink s
  | s == 0 = [1]
  | (even . length) t = [read l, read r]
  | otherwise = [s * 2024]
  where
    t = show s
    (l, r) = splitAt (length t `div` 2) t

flutter :: Int -> Int -> Int
flutter = memo2 flutter'
  where
    flutter' 0 _ = 1
    flutter' d s = sum $ map (flutter (d - 1)) (blink s)
