module Day18 where

type C = (Int, Int)

solveDay18 :: IO ()
solveDay18 = do
  example <- readFile "input/day18_example.txt"
  input <- readFile "input/day18_input.txt"

  putStrLn "\n\nDay 18\n---\nPart 1"
  print $ part1 (take 12 (parseInput example)) 6
  print $ part1 (take 1024 (parseInput input)) 70

  putStrLn "\nPart 2"
  print $ part2 (parseInput example) 6 12
  print $ part2 (parseInput input) 70 1024

parseInput :: String -> [C]
parseInput = map parseCoordinate . lines

parseCoordinate :: String -> C
parseCoordinate line =
  let (l, r) = break (== ',') line
   in (read l, read (tail r))

part1 :: [C] -> Int -> Int
part1 bytes dim = distances !! dim !! dim
  where
    distances = iterateDistances dim (emptyDistancesMap dim bytes)

part2 :: [C] -> Int -> Int -> C
part2 bytes dim start = bytes !! (index - 1)
  where
    index = binarySearch dim bytes (start, length bytes)

emptyDistancesMap :: Int -> [C] -> [[Int]]
emptyDistancesMap dim bytes = [[initDistance x y | x <- [0 .. dim]] | y <- [0 .. dim]]
  where
    initDistance x y
      | (x, y) == (0, 0) = 0
      | (x, y) `elem` bytes = -1
      | otherwise = dim * dim

iterateDistances :: Int -> [[Int]] -> [[Int]]
iterateDistances dim distances
  | distances == distances' = distances
  | otherwise = iterateDistances dim distances'
  where
    distances' =
      [ [ updatedDistance dim distances (x, y)
          | x <- [0 .. dim]
        ]
        | y <- [0 .. dim]
      ]

updatedDistance :: Int -> [[Int]] -> C -> Int
updatedDistance dim distances (x, y) = minimum (d : neighborDs)
  where
    d = distance distances (x, y)
    neighborDs = (map (+ 1) . filter (>= 0) . map (distance distances)) (neighbors dim (x, y))

distance :: [[Int]] -> C -> Int
distance distances (x, y) = distances !! y !! x

neighbors :: Int -> C -> [C]
neighbors dim (x, y) = filter within potentialNeighbors
  where
    within (x', y') = y' >= 0 && y' <= dim && x' >= 0 && x' <= dim
    potentialNeighbors = [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]

synthesizePath :: Int -> [[Int]] -> C -> Maybe [C]
synthesizePath dim distances (x, y)
  | d == 0 = Just [(x, y)]
  | null nextSteps = Nothing
  | otherwise = case nextSteps of
      [] -> Nothing
      (next : _) -> ((x, y) :) <$> synthesizePath dim distances next
  where
    d = distances !! y !! x
    nextSteps = filter (\(x', y') -> distances !! y' !! x' == d - 1) (neighbors dim (x, y))

binarySearch :: Int -> [C] -> (Int, Int) -> Int
binarySearch dim bytes (start, end)
  | start == end = start
  | otherwise = case synthesizePath dim distances (dim, dim) of
      Nothing -> binarySearch dim bytes (start, middle)
      Just _ -> binarySearch dim bytes (middle + 1, end)
  where
    middle = start + (end - start) `div` 2
    distances = iterateDistances dim (emptyDistancesMap dim (take middle bytes))
