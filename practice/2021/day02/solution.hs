import Data.Char (digitToInt)

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

parseInstruction :: String -> (Int, Int)
parseInstruction ('u' : ns) = (-(digitToInt $ last ns), 0)
parseInstruction ('d' : ns) = (digitToInt $ last ns, 0)
parseInstruction (_ : ns) = (0, digitToInt $ last ns)

part1 :: [String] -> Int
part1 ns = d * h
  where
    (d, h) = foldl1 (\(d1, h1) (d2, h2) -> (d1 + d2, h1 + h2)) $ map parseInstruction ns

part2 :: [String] -> Int
part2 ns = h * d
  where
    (_, h, d) = followInstruction ns (0, 0, 0)

followInstruction :: [String] -> (Int, Int, Int) -> (Int, Int, Int)
followInstruction [] v = v
followInstruction (('u' : n) : ns) (a, h, d) = followInstruction ns (a - digitToInt (last n), h, d)
followInstruction (('d' : n) : ns) (a, h, d) = followInstruction ns (a + digitToInt (last n), h, d)
followInstruction (('f' : n) : ns) (a, h, d) = followInstruction ns (a, h + x, d + a * x)
  where
    x = digitToInt (last n)
