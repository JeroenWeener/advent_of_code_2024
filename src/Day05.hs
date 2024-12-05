module Day05 where

solveDay05 :: IO ()
solveDay05 = do
  example <- readFile "input/day05_example.txt"
  input <- readFile "input/day05_input.txt"

  putStrLn "\n\nDay 05\n---\nPart 1"
  print $ part1 $ parseInput example
  print $ part1 $ parseInput input
  putStrLn "\nPart 2"
  print $ part2 $ parseInput example
  print $ part2 $ parseInput input

parseInput :: String -> ([(Int, Int)], [[Int]])
parseInput input = (parseOrders os, parseUpdates $ tail us)
  where
    (os, us) = break null $ lines input

parseOrders :: [String] -> [(Int, Int)]
parseOrders = map parseOrder

parseOrder :: String -> (Int, Int)
parseOrder o = (read l, read $ tail r)
  where
    (l, r) = splitAt 2 o

parseUpdates :: [String] -> [[Int]]
parseUpdates = map parseUpdate

parseUpdate :: String -> [Int]
parseUpdate line@[_, _] = [read line]
parseUpdate line = read (take 2 line) : parseUpdate (drop 3 line)

part1 :: ([(Int, Int)], [[Int]]) -> Int
part1 (os, us) = sum . map middle $ filter (isOrdered os) us

part2 :: ([(Int, Int)], [[Int]]) -> Int
part2 (os, us) = sum . map (middle . order os) $ filter (not . isOrdered os) us

middle :: [Int] -> Int
middle ns = ns !! (length ns `div` 2)

isOrdered :: [(Int, Int)] -> [Int] -> Bool
isOrdered _ [] = True
isOrdered os ns = all (isOrdered' ns (length ns)) os

isOrdered' :: [Int] -> Int -> (Int, Int) -> Bool
isOrdered' _ 0 _ = True
isOrdered' ns index (l, r) =
  restOrdered && case n of
    _
      | n == l -> r `notElem` take index ns
      | n == r -> l `notElem` drop index ns
      | otherwise -> True
  where
    index' = index - 1
    n = ns !! index'
    restOrdered = isOrdered' ns index' (l, r)

order :: [(Int, Int)] -> [Int] -> [Int]
order os ns = order' os' ns
  where
    os' = filter (\(l, r) -> l `elem` ns && r `elem` ns) os

order' :: [(Int, Int)] -> [Int] -> [Int]
order' _ [] = []
order' os ns = h : order' os' ns'
  where
    h = head $ filter (\n -> all (\(_, r) -> r /= n) os) ns
    os' = filter (\(l, _) -> l /= h) os
    ns' = filter (/= h) ns
