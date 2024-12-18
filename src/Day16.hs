module Day16 where

solveDay16 :: IO ()
solveDay16 = do
  example <- readFile "input/day16_example.txt"
  example2 <- readFile "input/day16_example2.txt"
  input <- readFile "input/day16_input.txt"

  putStrLn "\n\nDay 16\n---\nPart 1"
  print $ part1 $ parseInput example
  print $ part1 $ parseInput example2
  print $ part1 $ parseInput input

  putStrLn "\nPart 2"
  print $ part2 $ parseInput example
  print $ part2 $ parseInput example2
  print $ part2 $ parseInput input

parseInput :: String -> [String]
parseInput = lines

part1 :: [String] -> Int
part1 _ = 0

part2 :: [String] -> Int
part2 _ = 0
