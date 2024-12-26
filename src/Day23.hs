module Day23 where

import Data.List (intercalate, intersect, isInfixOf, maximumBy, nub, sort, union, (\\))
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)

solveDay23 :: IO ()
solveDay23 = do
  example <- readFile "input/day23_example.txt"
  input <- readFile "input/day23_input.txt"

  putStrLn "\n\nDay 23\n---\nPart 1"
  print $ part1 $ parseInput1 example
  print $ part1 $ parseInput1 input
  putStrLn "\nPart 2"
  print $ part2 $ parseInput2 example
  print $ part2 $ parseInput2 input

-- Part 1 --

type Computer = String

type Connection = (String, String)

parseInput1 :: String -> [Connection]
parseInput1 = map parseConnection . lines

parseConnection :: String -> Connection
parseConnection line = (take 2 line, drop 3 line)

part1 :: [Connection] -> Int
part1 input = length triplets `div` 3
  where
    triplets = [(a, b, c) | (a, b) <- input, c <- thirds input (a, b), containsT (a, b, c)]

    containsT :: (Computer, Computer, Computer) -> Bool
    containsT (a, b, c) = any (\e -> head e == 't') [a, b, c]

thirds :: [Connection] -> Connection -> [Computer]
thirds connections (a, b) = neighbors1 connections a `intersect` neighbors1 connections b

neighbors1 :: [Connection] -> Computer -> [Computer]
neighbors1 connections computer = [other (a, b) | (a, b) <- connections, a == computer || b == computer]
  where
    other :: Connection -> Computer
    other (a, b) = if a == computer then b else a

-- Part 2 --

type Graph = [(Vertex, [Vertex])]

type Vertex = String

parseInput2 :: String -> Graph
parseInput2 input = map (\computer -> (computer, parseNeighbors computer connections)) computers
  where
    connections = lines input
    computers = nub $ concatMap (\connection -> [take 2 connection, drop 3 connection]) connections

parseNeighbors :: Vertex -> [Vertex] -> [Vertex]
parseNeighbors computer = map other . filter (\connection -> computer `isInfixOf` connection)
  where
    other :: Vertex -> Vertex
    other connection = if a == computer then b else a
      where
        a = take 2 connection
        b = drop 3 connection

part2 :: Graph -> String
part2 = password . maximumClique
  where
    password = intercalate "," . sort

maximumClique :: Graph -> [Vertex]
maximumClique graph = maximumBy (comparing length) $ bronKerbosch graph [] vertices []
  where
    vertices = map fst graph

-- https://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm
bronKerbosch :: Graph -> [Vertex] -> [Vertex] -> [Vertex] -> [[Vertex]]
bronKerbosch graph r p x
  | null p && null x = [r]
  | otherwise =
      concatMap
        (\v -> bronKerbosch graph (v : r) (p `intersect` neighbors2 graph v) (x `intersect` neighbors2 graph v))
        (p \\ neighbors2 graph pivot)
  where
    pivot = choosePivot graph p x

choosePivot :: Graph -> [Vertex] -> [Vertex] -> Vertex
choosePivot graph p x = fst $ maximumBy (comparing snd) degrees
  where
    degrees = [(v, degree v) | v <- p `union` x]

    degree :: Vertex -> Int
    degree v = length (neighbors2 graph v)

neighbors2 :: Graph -> Vertex -> [Vertex]
neighbors2 graph v = fromMaybe [] (lookup v graph)
