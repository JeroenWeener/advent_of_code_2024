module Day20 where

solveDay20 :: IO ()
solveDay20 = do
  example <- readFile "input/day20_example.txt"
  input <- readFile "input/day20_input.txt"

  putStrLn "\n\nDay 20\n---\nPart 1"
  print $ part1 $ parseInput example
  print $ part1 $ parseInput input
  
  putStrLn "\nPart 2"
  print $ part2 $ parseInput example
  print $ part2 $ parseInput input

parseInput :: String -> [String]
parseInput = lines

part1 :: [String] -> Int
part1 _ = 0

part2 :: [String] -> Int
part2 _ = 0
