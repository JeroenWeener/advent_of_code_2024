import Data.List (group, sort)

type C = (Int, Int)

type L = (C, C)

main :: IO ()
main = do
  example <- readFile "example.txt"
  input <- readFile "input.txt"

  putStrLn "Part 1"
  print $ part1 $ parseInput example
  print $ part1 $ parseInput input
  putStrLn "\nPart 2"
  print $ part2 $ parseInput example
  print $ part2 $ parseInput input

parseInput :: String -> [L]
parseInput = map parseL . lines

parseL :: String -> L
parseL n = (a, b)
  where
    a = parseC $ takeWhile (/= '-') n
    b = parseC $ drop 1 $ dropWhile (/= '>') n

parseC :: String -> C
parseC n = (read x, read y)
  where
    x = takeWhile (/= ',') n
    y = drop 1 $ dropWhile (/= ',') n

part1 :: [L] -> Int
part1 ls = length $ filter (\g -> length g > 1) $ group $ sort $ concatMap pointsOnLine1 ls

part2 :: [L] -> Int
part2 ls = length $ filter (\g -> length g > 1) $ group $ sort $ concatMap pointsOnLine2 ls

pointsOnHorizontalLine :: Int -> Int -> Int -> [C]
pointsOnHorizontalLine x1 x2 y = [(x, y) | x <- [a .. b]]
  where
    a = min x1 x2
    b = max x1 x2

pointsOnVerticalLine :: Int -> Int -> Int -> [C]
pointsOnVerticalLine x y1 y2 = [(x, y) | y <- [a .. b]]
  where
    a = min y1 y2
    b = max y1 y2

pointsOnDiagonalLine :: Int -> Int -> Int -> Int -> [C]
pointsOnDiagonalLine x1 x2 y1 y2 = [(x, startY + sign * (x - minX)) | x <- [minX .. maxX]]
  where
    (minX, startY) = if x1 < x2 then (x1, y1) else (x2, y2)
    maxX = max x1 x2
    sign = if (x1 < x2) == (y1 < y2) then 1 else -1

pointsOnLine1 :: L -> [C]
pointsOnLine1 ((x1, y1), (x2, y2))
  | x1 == x2 = pointsOnVerticalLine x1 y1 y2
  | y1 == y2 = pointsOnHorizontalLine x1 x2 y1
  | otherwise = []

pointsOnLine2 :: L -> [C]
pointsOnLine2 ((x1, y1), (x2, y2))
  | x1 == x2 = pointsOnVerticalLine x1 y1 y2
  | y1 == y2 = pointsOnHorizontalLine x1 x2 y1
  | otherwise = pointsOnDiagonalLine x1 x2 y1 y2
