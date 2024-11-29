import Data.Char (digitToInt)
import Data.List (transpose)

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

parseInput :: String -> [String]
parseInput = lines

part1 :: [String] -> Int
part1 ns = gamma ns * epsilon ns

part2 :: [String] -> Int
part2 ns = ogr ns * csr ns

gamma :: [String] -> Int
gamma = binaryToInt . mcbs

epsilon :: [String] -> Int
epsilon = binaryToInt . lcbs

ogr :: [String] -> Int
ogr ns = binaryToInt $ f mcbs ns 0

csr :: [String] -> Int
csr ns = binaryToInt $ f lcbs ns 0

mcbs :: [String] -> String
mcbs = map mcb . transpose

lcbs :: [String] -> String
lcbs = map lcb . transpose

mcb :: String -> Char
mcb cs = if count '0' cs > count '1' cs then '0' else '1'

lcb :: String -> Char
lcb cs = if count '0' cs > count '1' cs then '1' else '0'

count :: Char -> String -> Int
count c = length . filter (== c)

binaryToInt :: String -> Int
binaryToInt ns = foldl (\a n -> 2 * a + n) 0 $ map digitToInt ns

f :: ([String] -> String) -> [String] -> Int -> String
f _ [n] _ = n
f fBits ns p = f fBits (filter (\n -> n !! p == bs !! p) ns) (p + 1)
  where
    bs = fBits ns
