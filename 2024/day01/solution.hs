import Data.List (sort, transpose)

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
parseInput input = transpose $ map (map read . words) (lines input)

part1 :: [[Int]] -> Int
part1 input = sum $ map d $ transpose $ map sort input

part2 :: [[Int]] -> Int
part2 [as, bs] = sum $ map (\a -> a * count a bs) as

d :: [Int] -> Int
d [a, b] = abs (a - b)

count :: Int -> [Int] -> Int
count n = length . filter (== n)
