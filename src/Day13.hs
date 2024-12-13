module Day13 where

import Data.Char (isDigit)
import Data.List.Split (splitOn)

type C = (Int, Int)

solveDay13 :: IO ()
solveDay13 = do
  example <- readFile "input/day13_example.txt"
  input <- readFile "input/day13_input.txt"

  putStrLn "\n\nDay 13\n---\nPart 1"
  print $ part1 $ parseInput example
  print $ part1 $ parseInput input

  putStrLn "\nPart 2"
  print $ part2 $ parseInput example
  print $ part2 $ parseInput input

parseInput :: String -> [(C, C, C)]
parseInput input = map parseMachine (splitOn "\n\n" input)

parseMachine :: String -> (C, C, C)
parseMachine m = case map parseLine (lines m) of
  [a, b, c] -> (a, b, c)
  _ -> error ""

parseLine :: String -> C
parseLine line = case extractInts line of
  [a, b] -> (a, b)
  _ -> error ""

extractInts :: String -> [Int]
extractInts xs = do
  let s = dropWhile (not . isDigit) xs
  let i = takeWhile isDigit s
  let r = dropWhile isDigit s
  if null i
    then []
    else read i : extractInts r

part1 :: [(C, C, C)] -> Int
part1 = sum . map (tokens . intersection)

part2 :: [(C, C, C)] -> Int
part2 = sum . map (tokens . intersection . translate)

{-
Given (x1,x2), (x2,y2), (x3,y3), find a and b such that:

a * x1 + b * x2 = x3
a * y1 + b * y2 = y3.

Solve for b:
a = (x3 - b * x2) / x1
a = (y3 - b * y2) / y1
(x3 - b * x2) / x1 = (y3 - b * y2) / y1
y1(x3 - b * x2) = x1(y3 - b * y2)
y1 * x3 - y1 * x2 * b = x1 * y3 - x1 * y2 * b
(x1 * y2 - y1 * x2)b = x1 * y3 - y1 * x3
b = (x1 * y3 - y1 * x3) / (x1 * y2 - y1 * x2)
a = (x3 - b * x2) / x1

Solutions are valid only if a and b are integers.
-}
intersection :: (C, C, C) -> Maybe C
intersection ((x1, y1), (x2, y2), (x3, y3)) = if bIsInteger && aIsInteger then Just (a, b) else Nothing
  where
    c = x1 * y3 - y1 * x3
    d = x1 * y2 - y1 * x2
    bIsInteger = c `mod` d == 0
    b = c `div` d
    e = x3 - b * x2
    f = x1
    aIsInteger = e `mod` f == 0
    a = e `div` f

tokens :: Maybe C -> Int
tokens Nothing = 0
tokens (Just (a, b)) = 3 * a + b

translate :: (C, C, C) -> (C, C, C)
translate (c1, c2, (x, y)) = (c1, c2, (x + 10000000000000, y + 10000000000000))
