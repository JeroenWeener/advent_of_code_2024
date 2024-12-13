module Day12 where

import Data.List (groupBy, sortOn, (\\))

type C = (Int, Int)

solveDay12 :: IO ()
solveDay12 = do
  example <- readFile "input/day12_example.txt"
  input <- readFile "input/day12_input.txt"

  putStrLn "\n\nDay 12\n---\nPart 1"
  print $ part1 $ parseInput example
  print $ part1 $ parseInput input

  putStrLn "\nPart 2"
  print $ part2 $ parseInput example
  print $ part2 $ parseInput input

parseInput :: String -> [(C, Char)]
parseInput input =
  [ ((y, x), c)
    | y <- [0 .. size],
      x <- [0 .. size],
      let c = (lines input !! y) !! x
  ]
  where
    size = length (lines input) - 1

part1 :: [(C, Char)] -> Int
part1 =
  sum
    . map (price (length . concat . perimeter))
    . partition

part2 :: [(C, Char)] -> Int
part2 =
  sum
    . map (price (length . sides . perimeter))
    . partition

price :: ([C] -> Int) -> [C] -> Int
price f r = length r * f r

(+:) :: C -> C -> C
(+:) (a, b) (c, d) = (a + c, b + d)

directions :: [C]
directions =
  [ (1, 0),
    (0, 1),
    (-1, 0),
    (0, -1)
  ]

groupOn :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy (\a b -> f a == f b)

partition :: [(C, Char)] -> [[C]]
partition =
  concatMap (subpartition . map fst)
    . groupOn snd
    . sortOn snd

subpartition :: [C] -> [[C]]
subpartition [] = []
subpartition (g : gs) = part : subpartition rest
  where
    part = flood gs [g]
    rest = gs \\ part

flood :: [C] -> [C] -> [C]
flood _ [] = []
flood remaining (q : qs) = q : flood remaining' qs'
  where
    surrounding = [q +: d | d <- directions]
    ns = filter (`elem` remaining) surrounding
    remaining' = remaining \\ ns
    qs' = qs ++ ns

perimeter :: [C] -> [[C]]
perimeter r = map (subperimeter r) directions

subperimeter :: [C] -> C -> [C]
subperimeter r d = filter (`notElem` r) $ map (+: d) r

sides :: [[C]] -> [C]
sides = concatMap subsides

subsides :: [C] -> [C]
subsides [] = []
subsides (x : xs)
  | c == 0 = x : subsides xs
  | c == 1 = subsides xs
  | otherwise = subsides (xs ++ [x])
  where
    ns = [x +: d | d <- directions]
    c = length $ filter (`elem` xs) ns
