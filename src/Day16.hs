module Day16 where

import Data.Heap (MinPrioHeap)
import qualified Data.Heap as Heap
import Data.List (nub)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

data Distance = Dist Int | Infinity deriving (Show, Eq, Ord)

type DijkstraState =
  ( [V],
    Map.Map V (Distance, [[V]]),
    MinPrioHeap Distance V
  )

type V = (Int, Int)

type E = (V, V, Distance)

{--
Dijkstra's algorithm adapted from https://mmhaskell.com/blog/2022/8/22/dijkstras-algorithm-in-haskell.
--}
solveDay16 :: IO ()
solveDay16 = do
  example <- readFile "input/day16_example.txt"
  example2 <- readFile "input/day16_example2.txt"
  input <- readFile "input/day16_input.txt"

  let (result1Example, result2Example) = bothParts $ parseInput example
  let (result1Example2, result2Example2) = bothParts $ parseInput example2
  let (result1Input, result2Input) = bothParts $ parseInput input

  putStrLn "\n\nDay 16\n---\nPart 1"
  print result1Example
  print result1Example2
  print result1Input

  putStrLn "\nPart 2"
  print result2Example
  print result2Example2
  print result2Input

bothParts :: (Int, [E]) -> (Distance, Int)
bothParts (dim, edges) = (shortestDistance, countTiles paths)
  where
    source = (dim - 2, 1)
    target = (1, dim - 2)
    (shortestDistance, paths) = dijkstra edges source target ?! target

parseInput :: String -> (Int, [E])
parseInput input = (length ls, edges)
  where
    ls = lines input
    vertices = parseVertices ls
    edges = parseEdges ls vertices

parseVertices :: [String] -> [V]
parseVertices ls = [(y, x) | x <- [1 .. length ls - 2], y <- [1 .. length ls - 2], isVertex (y, x)]
  where
    isVertex c = isSpecial c || (isTile c && hasHorizontalNeighbors c && hasVerticalNeighbors c)
    isSpecial (y, x) = ls !! y !! x `elem` "ES"
    isTile (y, x) = ls !! y !! x == '.'
    ls' = map (map (\c -> if c == '#' then '#' else '.')) ls
    hasHorizontalNeighbors (y, x) = ls' !! y !! (x - 1) == '.' || ls' !! y !! (x + 1) == '.'
    hasVerticalNeighbors (y, x) = ls' !! (y - 1) !! x == '.' || ls' !! (y + 1) !! x == '.'

parseEdges :: [String] -> [V] -> [E]
parseEdges ls vs =
  [ (v1, v2, cost)
    | v1 <- vs,
      v2 <- vs,
      v1 < v2,
      let cost = parseEdgeCost ls v1 v2,
      cost /= Infinity
  ]

parseEdgeCost :: [String] -> V -> V -> Distance
parseEdgeCost ls (y1, x1) (y2, x2)
  | y1 == y2 = xCost y1 (min x1 x2) (max x1 x2)
  | x1 == x2 = yCost x1 (min y1 y2) (max y1 y2)
  | otherwise = Infinity
  where
    isWall y x = ls !! y !! x == '#'
    xCost y xa xb
      | xa == xb = Dist 0
      | isWall y xa = Infinity
      | otherwise = Dist 1 +: xCost y (xa + 1) xb
    yCost x ya yb
      | ya == yb = Dist 0
      | isWall ya x = Infinity
      | otherwise = Dist 1 +: yCost x (ya + 1) yb

neighbors :: V -> [E] -> [(V, Distance)]
neighbors _ [] = []
neighbors v ((v1, v2, c) : edges)
  | v == v1 = (v2, c) : neighbors v edges
  | v == v2 = (v1, c) : neighbors v edges
  | otherwise = neighbors v edges

dijkstra :: [E] -> V -> V -> Map.Map V (Distance, [[V]])
dijkstra edges source target = processQueue initialState
  where
    initialVisited = []
    initialDistances = Map.singleton source (Dist 0, [[source]])
    initialQueue = Heap.fromList [(Dist 0, source)]
    initialState = (initialVisited, initialDistances, initialQueue)

    processQueue :: DijkstraState -> Map.Map V (Distance, [[V]])
    processQueue (v0, d0, q0) = case Heap.view q0 of
      Nothing -> d0
      Just ((_, vertex), q1) ->
        if vertex == target
          then d0
          else
            if vertex `elem` v0
              then processQueue (v0, d0, q1)
              else
                let v1 = vertex : v0
                    allNeighbors = neighbors vertex edges
                    unvisitedNeighbors = filter (\(n, _) -> n `notElem` v1) allNeighbors
                 in processQueue $ foldl (foldNeighbor vertex) (v1, d0, q1) unvisitedNeighbors

    foldNeighbor :: V -> DijkstraState -> (V, Distance) -> DijkstraState
    foldNeighbor current ds@(v1, d0, q1) (neighborVertex, cost) =
      let (currentDistance, currentPaths) = d0 ?! current
          (neighborDistance, neighborPaths) = d0 ?! neighborVertex
          -- Incur turning cost unless the first hop is horizontal
          turningCost = if source == current && fst neighborVertex == fst current then Dist 0 else Dist 1000
          altDistance = currentDistance +: cost +: turningCost
       in if altDistance < neighborDistance
            then
              ( v1,
                Map.insert neighborVertex (altDistance, map (neighborVertex :) currentPaths) d0,
                Heap.insert (altDistance, neighborVertex) q1
              )
            else
              if altDistance == neighborDistance
                then
                  ( v1,
                    Map.insert neighborVertex (altDistance, map (neighborVertex :) currentPaths ++ neighborPaths) d0,
                    q1
                  )
                else ds

countTiles :: [[V]] -> Int
countTiles = length . nub . concatMap tiles

tiles :: [V] -> [(Int, Int)]
tiles ((y1, x1) : (y2, x2) : vs) =
  [(y, x) | y <- [min y1 y2 .. max y1 y2], x <- [min x1 x2 .. max x1 x2]]
    ++ tiles ((y2, x2) : vs)
tiles _ = []

(+:) :: Distance -> Distance -> Distance
(+:) (Dist a) (Dist b) = Dist (a + b)
(+:) _ _ = Infinity

(?!) :: Map.Map V (Distance, [[V]]) -> V -> (Distance, [[V]])
(?!) distanceMap key = fromMaybe (Infinity, []) (Map.lookup key distanceMap)
