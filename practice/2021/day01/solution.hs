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

parseInput :: String -> [Int]
parseInput input = map read $ words input

part1 :: [Int] -> Int
part1 ns = length $ filter (< 0) $ zipWith (-) ns (drop 1 ns)

part2 :: [Int] -> Int
part2 ns = length $ filter (< 0) $ zipWith (-) (swSums ns) (swSums (drop 1 ns))

swSums :: [Int] -> [Int]
swSums ns = map (\(a, b, c) -> a + b + c) $ zip3 ns (drop 1 ns) (drop 2 ns)
