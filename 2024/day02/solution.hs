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

parseInput :: String -> [[Int]]
parseInput input = map (map read . words) (lines input)

part1 :: [[Int]] -> Int
part1 = length . filter safe

part2 :: [[Int]] -> Int
part2 rs = length . filter (any safe) $ map dampen rs

safe :: [Int] -> Bool
safe rs = safeIncrease rs || safeDecrease rs

safeIncrease :: [Int] -> Bool
safeIncrease [r] = True
safeIncrease (r1 : r2 : rs) = d > 0 && d <= 3 && safeIncrease (r2 : rs)
  where
    d = r2 - r1

safeDecrease :: [Int] -> Bool
safeDecrease [r] = True
safeDecrease (r1 : r2 : rs) = d > 0 && d <= 3 && safeDecrease (r2 : rs)
  where
    d = r1 - r2

dampen :: [Int] -> [[Int]]
dampen rs = [removeAt rs i | i <- [0 .. length rs - 1]]

removeAt :: [Int] -> Int -> [Int]
removeAt rs i = left ++ right
  where
    (left, _ : right) = splitAt i rs
