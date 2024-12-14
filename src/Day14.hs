module Day14 where

import Data.Char (isDigit)
import Data.List (nub)

type C = (Int, Int)

type R = (C, C)

solveDay14 :: IO ()
solveDay14 = do
  example <- readFile "input/day14_example.txt"
  input <- readFile "input/day14_input.txt"

  putStrLn "\n\nDay 14\n---\nPart 1"
  print $ part1 (7, 11) $ parseInput example
  print $ part1 (103, 101) $ parseInput input

  putStrLn "\nPart 2"
  let (index, picture) = part2 $ parseInput input
  print index
  mapM_ putStrLn picture

parseInput :: String -> [R]
parseInput = map parseLine . lines

parseLine :: String -> R
parseLine l = case extractInts l of
  [x, y, dx, dy] -> ((y, x), (dy, dx))
  _ -> error ""

extractInts :: String -> [Int]
extractInts xs = do
  let s = dropWhile (\x -> (not . isDigit) x && x /= '-') xs
  let m = if null s || (head s /= '-') then "" else "-"
  let s' = if m == "" then s else drop 1 s
  let i = takeWhile isDigit s'
  let r = dropWhile isDigit s'
  if null i
    then []
    else read (m ++ i) : extractInts r

part1 :: C -> [R] -> Int
part1 dims robots = countRobots dims (iterate (map (updateRobot dims)) robots !! 100)

part2 :: [R] -> (Int, [String])
part2 robots = (index, picture)
  where
    dims = (103, 101)
    configurations = zip [0 ..] (iterate (map (updateRobot dims)) robots)
    (index, configuration) =
      ( head
          . filter (\(_, c) -> totalVerticalScore dims c < 3000)
          . filter (\(_, c) -> nonOverlapping c)
      )
        configurations
    picture = showConfiguration dims configuration

updateRobot :: C -> R -> R
updateRobot (h, w) ((y, x), (dy, dx)) = (((y + dy) `mod` h, (x + dx) `mod` w), (dy, dx))

countRobots :: C -> [R] -> Int
countRobots (h, w) robots = tl * tr * ll * lr
  where
    middleY = h `div` 2
    middleX = w `div` 2
    tl = (length . filter (\((y, x), _) -> y < middleY && x < middleX)) robots
    tr = (length . filter (\((y, x), _) -> y < middleY && x > middleX)) robots
    ll = (length . filter (\((y, x), _) -> y > middleY && x < middleX)) robots
    lr = (length . filter (\((y, x), _) -> y > middleY && x > middleX)) robots

nonOverlapping :: [R] -> Bool
nonOverlapping robots = length robots == (length . nub . map fst) robots

totalVerticalScore :: C -> [R] -> Int
totalVerticalScore (h, w) robots =
  sum
    [ verticalScore w (filter (\((y', _), _) -> y == y') robots)
      | y <- [0 .. h]
    ]

verticalScore :: Int -> [R] -> Int
verticalScore width = abs . sum . map (\((_, x), _) -> x - width `div` 2)

showConfiguration :: C -> [R] -> [String]
showConfiguration (h, w) rs = [[if (y, x) `elem` map fst rs then 'X' else ' ' | x <- [0 .. w]] | y <- [0 .. h]]
