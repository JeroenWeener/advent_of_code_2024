module Day20 where

import Data.List (find)
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Maybe (fromJust)

type C = (Int, Int)

solveDay20 :: IO ()
solveDay20 = do
  example <- readFile "input/day20_example.txt"
  input <- readFile "input/day20_input.txt"

  putStrLn "\n\nDay 20\n---\nPart 1"
  print $ part1 4 $ parseInput example
  print $ part1 100 $ parseInput input

  putStrLn "\nPart 2"
  print $ part2 50 $ parseInput example
  print $ part2 100 $ parseInput input

parseInput :: String -> ([C], C, C)
parseInput input =
  let ls = lines input
   in (parsePaths ls, parseSymbol ls 'S', parseSymbol ls 'E')

parsePaths :: [String] -> [C]
parsePaths ls =
  [ (y, x)
    | y <- [0 .. length ls - 1],
      x <- [0 .. length ls - 1],
      (ls !! y !! x) `elem` ".SE"
  ]

parseSymbol :: [String] -> Char -> C
parseSymbol ls symbol =
  let i = (fst . fromJust . find (\(_, c) -> c == symbol) . zip [0 ..]) (concat ls)
   in (i `div` length ls, i `mod` length ls)

part1 :: Int -> ([C], C, C) -> Int
part1 timeSave (paths, start, end) = cheats distanceMap paths timeSave 2
  where
    distanceMap = Map.fromList $ distances paths [] start end 0

part2 :: Int -> ([C], C, C) -> Int
part2 timeSave (paths, start, end) = cheats distanceMap paths timeSave 20
  where
    distanceMap = Map.fromList $ distances paths [] start end 0

distances :: [C] -> [C] -> C -> C -> Int -> [(C, Int)]
distances paths visited current target distance
  | current == target = [(current, distance)]
  | otherwise = (current, distance) : distances paths (current : visited) (next paths visited current) target (distance + 1)

next :: [C] -> [C] -> C -> C
next paths visited (y, x) = head $ filter (\n -> n `notElem` visited && n `elem` paths) ns
  where
    ns = [(y - 1, x), (y, x + 1), (y + 1, x), (y, x - 1)]

cheats :: Map.Map C Int -> [C] -> Int -> Int -> Int
cheats distanceMap paths timeSave cheatLength =
  length
    [ ()
      | p1 <- paths,
        p2 <- paths,
        p1 /= p2,
        let md = manhattan p1 p2,
        md <= cheatLength,
        let d1 = distanceMap ! p1,
        let d2 = distanceMap ! p2,
        d2 - d1 >= timeSave + md
    ]

manhattan :: C -> C -> Int
manhattan (y1, x1) (y2, x2) = abs (y2 - y1) + abs (x2 - x1)
