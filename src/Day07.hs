module Day07 where

solveDay07 :: IO ()
solveDay07 = do
  example <- readFile "input/day07_example.txt"
  input <- readFile "input/day07_input.txt"

  putStrLn "\n\nDay 07\n---\nPart 1"
  print $ part1 $ parseInput example
  print $ part1 $ parseInput input
  putStrLn "\nPart 2"
  print $ part2 $ parseInput example
  print $ part2 $ parseInput input

parseInput :: String -> [(Int, [Int])]
parseInput = map parseLine . lines

parseLine :: String -> (Int, [Int])
parseLine line = (ans, ns)
  where
    (a, b) = break (== ':') line
    ans = read a
    ns = map read $ words (drop 2 b)

part1 :: [(Int, [Int])] -> Int
part1 = sum . map fst . filter (isSolvable ops 0)
  where
    ops = [(+), (*)]

part2 :: [(Int, [Int])] -> Int
part2 = sum . map fst . filter (isSolvable ops 0)
  where
    conc a b = read (show a ++ show b)
    ops = [(+), (*), conc]

isSolvable :: [Int -> Int -> Int] -> Int -> (Int, [Int]) -> Bool
isSolvable _ cur (ans, []) = ans == cur
isSolvable ops cur (ans, n : ns)
  | cur > ans = False
  | otherwise = any (\op -> isSolvable ops (op cur n) (ans, ns)) ops
