module Day03 where

import Text.Regex.TDFA

solveDay03 :: IO ()
solveDay03 = do
  example <- readFile "input/day03_example.txt"
  example2 <- readFile "input/day03_example2.txt"
  input <- readFile "input/day03_input.txt"

  putStrLn "\n\nDay 03\n---\nPart 1"
  print $ part1 example
  print $ part1 input
  putStrLn "\nPart 2"
  print $ part2 example2
  print $ part2 input

regex :: String
regex = "mul\\([0-9]+,[0-9]+\\)"

regex2 :: String
regex2 = "mul\\([0-9]+,[0-9]+\\)|do\\(\\)|don't\\(\\)"

part1 :: String -> Int
part1 input = sum . map parseMul $ getAllTextMatches (input =~ regex)

part2 :: String -> Int
part2 input = parseInstructions True $ getAllTextMatches (input =~ regex2)

parseMul :: String -> Int
parseMul text = a * b
  where
    a = read $ takeWhile (/= ',') $ drop 4 text
    b = read $ init $ drop 1 $ dropWhile (/= ',') text

parseInstructions :: Bool -> [String] -> Int
parseInstructions _ [] = 0
parseInstructions _ ("do()" : ts) = parseInstructions True ts
parseInstructions _ ("don't()" : ts) = parseInstructions False ts
parseInstructions False (_ : ts) = parseInstructions False ts
parseInstructions True (mul : ts) = parseMul mul + parseInstructions True ts
