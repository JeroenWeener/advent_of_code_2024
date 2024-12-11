module Day10 where

import Data.Char (digitToInt)
import Data.List (nub)
import qualified Data.Map as Map (Map, fromList, keys, lookup)
import Data.Maybe (fromMaybe)

type C = (Int, Int)

type T = [C]

type G = Map.Map C Int

solveDay10 :: IO ()
solveDay10 = do
  example <- readFile "input/day10_example.txt"
  input <- readFile "input/day10_input.txt"

  putStrLn "\n\nDay 10\n---\nPart 1"
  print $ part1 $ parseInput example
  print $ part1 $ parseInput input
  putStrLn "\nPart 2"
  print $ part2 $ parseInput example
  print $ part2 $ parseInput input

parseInput :: String -> G
parseInput input =
  Map.fromList
    [ ((y, x), h)
      | y <- [0 .. dim],
        x <- [0 .. dim],
        let h = digitToInt $ ls !! y !! x
    ]
  where
    ls = lines input
    dim = length ls - 1

part1 :: G -> Int
part1 grid = length . concatMap (nub . trails grid 0) $ trailHeads grid

part2 :: G -> Int
part2 grid = sum . map (length . trails grid 0) $ trailHeads grid

directions :: [C]
directions = [(1, 0), (0, 1), (-1, 0), (0, -1)]

(+:) :: C -> C -> C
(+:) (a, b) (c, d) = (a + c, b + d)

trails :: G -> Int -> C -> [C]
trails _ 9 c = [c]
trails grid h p = concat [trails grid (h + 1) (y, x) | (y, x) <- ns, height (y, x) grid == (h + 1)]
  where
    ns = [p +: d | d <- directions]

trailHeads :: G -> [C]
trailHeads grid = [p | p <- Map.keys grid, height p grid == 0]

height :: C -> G -> Int
height p grid = fromMaybe (-1) $ Map.lookup p grid
