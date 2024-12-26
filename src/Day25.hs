module Day25 where

import Data.List (elemIndex, transpose)
import Data.Maybe (fromJust)

solveDay25 :: IO ()
solveDay25 = do
  example <- readFile "input/day25_example.txt"
  input <- readFile "input/day25_input.txt"

  putStrLn "\n\nDay 25\n---\nPart 1"
  print $ part1 $ parseInput example
  print $ part1 $ parseInput input

part1 :: ([[Int]], [[Int]]) -> Int
part1 (keys, locks) = sum [1 | k <- keys, l <- locks, not $ overlap k l]

parseInput :: String -> ([[Int]], [[Int]])
parseInput input =
  let sections = parseSections (lines input)
      keys = filter (\s -> (head . head) s == '.') sections
      locks = filter (\s -> (head . head) s == '#') sections
   in (map parseKey keys, map parseLock locks)

parseSections :: [String] -> [[String]]
parseSections ls =
  let (section, rest) = break null ls
   in section : case rest of
        [] -> []
        (_ : xs) -> parseSections xs

parseKey :: [String] -> [Int]
parseKey ls = map ((6 -) . fromJust . elemIndex '#') (transpose ls)

parseLock :: [String] -> [Int]
parseLock ls = map ((-1 +) . fromJust . elemIndex '.') (transpose ls)

overlap :: [Int] -> [Int] -> Bool
overlap key lock = any (> 5) (zipWith (+) key lock)
