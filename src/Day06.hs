module Day06 where

import Data.List (find)
import Data.Maybe (fromJust)
import qualified Data.Set as Set

type C = (Int, Int)

type D = Int

type G = (C, D)

solveDay06 :: IO ()
solveDay06 = do
  example <- readFile "input/day06_example.txt"
  input <- readFile "input/day06_input.txt"

  putStrLn "\n\nDay 06\n---\nPart 1"
  print $ part1 $ parseInput example
  print $ part1 $ parseInput input
  putStrLn "\nPart 2"
  print $ part2 $ parseInput example
  print $ part2 $ parseInput input

parseInput :: String -> (Set.Set C, G, (Int, Int))
parseInput input = (parseObstacles ls, parseGuard ls, parseDims ls)
  where
    ls = lines input

parseDims :: [String] -> (Int, Int)
parseDims ls = (length (head ls), length ls)

parseObstacles :: [String] -> Set.Set C
parseObstacles ls =
  Set.fromList
    [ (x, y)
      | x <- [0 .. length (head ls) - 1],
        y <- [0 .. length ls - 1],
        ls !! y !! x == '#'
    ]

parseGuard :: [String] -> G
parseGuard ls = ((x, y), 0)
  where
    width = length (head ls)
    i = fst . fromJust . find (\(_, c) -> c == '^') $ zip [0 ..] (concat ls)
    x = i `mod` width
    y = i `div` width

part1 :: (Set.Set C, G, (Int, Int)) -> Int
part1 (os, g, dims) = steps os vs g dims
  where
    vs = Set.singleton g

part2 :: (Set.Set C, G, (Int, Int)) -> Int
part2 (os, g, dims) = loops os vs g dims False
  where
    vs = Set.singleton g

direction :: D -> C
direction 0 = (0, -1)
direction 1 = (1, 0)
direction 2 = (0, 1)
direction _ = (-1, 0)

turnRight :: D -> D
turnRight n = (n + 1) `mod` 4

add :: C -> C -> C
add (l1, r1) (l2, r2) = (l1 + l2, r1 + r2)

steps :: Set.Set C -> Set.Set (C, D) -> G -> (Int, Int) -> Int
steps os vs (p, d) dims
  | isOut = length (Set.map fst vs)
  | isObstructed = turn
  | otherwise = move
  where
    p'@(x, y) = p `add` direction d
    isOut = x < 0 || x >= fst dims || y < 0 || y >= snd dims
    isObstructed = p' `elem` os
    vs' = Set.insert (p', d) vs
    turn = steps os vs (p, turnRight d) dims
    move = steps os vs' (p', d) dims

loops :: Set.Set C -> Set.Set (C, D) -> G -> (Int, Int) -> Bool -> Int
loops os vs (p, d) dims e
  | isOut = 0
  | isVisited = 1
  | isObstructed = turn
  | otherwise = move + placeObstacle
  where
    p'@(x, y) = p `add` direction d
    isOut = x < 0 || x >= fst dims || y < 0 || y >= snd dims
    isVisited = (p', d) `elem` vs
    isObstructed = p' `elem` os
    turn = loops os vs (p, turnRight d) dims e
    move = loops os vs' g' dims e
      where
        g' = (p', d)
        vs' = Set.insert g' vs
    placeObstacle
      | e = 0
      | p' `elem` Set.map fst vs = 0
      | otherwise = loops os' vs (p, turnRight d) dims True
      where
        os' = Set.insert p' os
