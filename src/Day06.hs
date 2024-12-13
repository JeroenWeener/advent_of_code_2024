module Day06 where

import Data.List (find, nub, sortOn, unfoldr)
import Data.Maybe (fromJust, isNothing)

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

parseInput :: String -> ([C], G, Int)
parseInput input = (parseObstacles ls, parseGuard ls, length ls)
  where
    ls = lines input

parseObstacles :: [String] -> [C]
parseObstacles ls =
  [ (y, x)
    | y <- [0 .. length ls - 1],
      x <- [0 .. length ls - 1],
      ls !! y !! x == '#'
  ]

parseGuard :: [String] -> G
parseGuard ls = ((y, x), 0)
  where
    i =
      ( fst
          . fromJust
          . find (\(_, c) -> c == '^')
          . zip [0 ..]
      )
        (concat ls)
    x = i `mod` length ls
    y = i `div` length ls

part1 :: ([C], G, Int) -> Int
part1 (obstacle, guard, size) = (length . steps obstacle size) guard

part2 :: ([C], G, Int) -> Int
part2 (obstacle, guard, size) = (length . filter (isLooping guard [])) obstacles
  where
    obstacles = [o : obstacle | o <- steps obstacle size guard]

steps :: [C] -> Int -> G -> [C]
steps obstacle size = nub . map fst . unfoldr (step obstacle size)

step :: [C] -> Int -> G -> Maybe (G, G)
step obstacle size guard@(p, d)
  | isOut p size = Nothing
  | next `elem` obstacle = Just (guard, (p, turn d))
  | otherwise = Just (guard, (next, d))
  where
    next = p +: direction d

isLooping :: G -> [G] -> [C] -> Bool
isLooping guard@(_, d) visited obstacle
  | guard `elem` visited = True
  | isNothing nextObstacle = False
  | otherwise = isLooping (nextPosition, turn d) (guard : visited) obstacle
  where
    nextObstacle = findObstacle guard obstacle
    nextPosition = fromJust nextObstacle -: direction d

findObstacle :: G -> [C] -> Maybe C
findObstacle ((y1, x1), 0) = maybeLast . sortOn fst . filter (\(y2, x2) -> x1 == x2 && y2 < y1)
findObstacle ((y1, x1), 1) = maybeHead . sortOn snd . filter (\(y2, x2) -> y1 == y2 && x2 > x1)
findObstacle ((y1, x1), 2) = maybeHead . sortOn fst . filter (\(y2, x2) -> x1 == x2 && y2 > y1)
findObstacle ((y1, x1), _) = maybeLast . sortOn snd . filter (\(y2, x2) -> y1 == y2 && x2 < x1)

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead n = Just (head n)

maybeLast :: [a] -> Maybe a
maybeLast [] = Nothing
maybeLast n = Just (last n)

isOut :: C -> Int -> Bool
isOut (y, x) size = y < 0 || y >= size || x < 0 || x >= size

(+:) :: C -> C -> C
(+:) (a, b) (c, d) = (a + c, b + d)

(-:) :: C -> C -> C
(-:) (a, b) (c, d) = (a - c, b - d)

turn :: Int -> Int
turn n = (n + 1) `mod` 4

direction :: Int -> C
direction 0 = (-1, 0)
direction 1 = (0, 1)
direction 2 = (1, 0)
direction _ = (0, -1)
