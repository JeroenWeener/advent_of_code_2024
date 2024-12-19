module Day19 where

import Data.List (isPrefixOf)
import Data.MemoTrie (memo)

solveDay19 :: IO ()
solveDay19 = do
  example <- readFile "input/day19_example.txt"
  input <- readFile "input/day19_input.txt"

  putStrLn "\n\nDay 19\n---\nPart 1"
  print $ part1 $ parseInput example
  print $ part1 $ parseInput input

  putStrLn "\nPart 2"
  print $ part2 $ parseInput example
  print $ part2 $ parseInput input

parseInput :: String -> ([String], [String])
parseInput input = (towels, designs)
  where
    ls = lines input
    towels = parseTowels (head ls)
    designs = drop 2 ls

parseTowels :: String -> [String]
parseTowels [] = []
parseTowels s = towel : parseTowels (drop 2 s')
  where
    (towel, s') = break (== ',') s

part1 :: ([String], [String]) -> Int
part1 (towels, designs) = length $ filter (isPossible towels) designs

part2 :: ([String], [String]) -> Int
part2 (towels, designs) = sum $ map (arrangements towels) designs

isPossible :: [String] -> String -> Bool
isPossible _ [] = True
isPossible towels design = any (\towel -> isPossible towels (drop (length towel) design)) nextTowels
  where
    nextTowels = filter (`isPrefixOf` design) towels

arrangements :: [String] -> String -> Int
arrangements towels = memo arrangements'
  where
    arrangements' [] = 1
    arrangements' design = sum $ map (\towel -> arrangements towels (drop (length towel) design)) nextTowels
      where
        nextTowels = filter (`isPrefixOf` design) towels
