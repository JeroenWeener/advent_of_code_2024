import Control.Applicative ((<|>))
import Data.List (transpose)
import Data.Maybe (fromJust, isJust)

type Board = [[Int]]

main :: IO ()
main = do
  example <- readFile "example.txt"
  input <- readFile "input.txt"

  print $ parseInput example

  putStrLn "Part 1"
  print $ part1 $ parseInput example
  print $ part1 $ parseInput input
  putStrLn "\nPart 2"
  print $ part2 $ parseInput example
  print $ part2 $ parseInput input

parseInput :: String -> ([Int], [Board])
parseInput input = (parseDraws $ head $ lines input, map parseBoard $ parseBoards $ drop 2 $ lines input)

parseDraws :: String -> [Int]
parseDraws [] = []
parseDraws ns = read (takeWhile (/= ',') ns) : parseDraws (drop 1 $ dropWhile (/= ',') ns)

parseBoards :: [String] -> [[String]]
parseBoards [] = []
parseBoards ns = takeWhile (/= "") ns : parseBoards (drop 1 $ dropWhile (/= "") ns)

parseBoard :: [String] -> Board
parseBoard = map parseBoardRow

parseBoardRow :: String -> [Int]
parseBoardRow row = map parseIntString $ words row

parseIntString :: String -> Int
parseIntString = read

part1 :: ([Int], [Board]) -> Int
part1 (draws, boards) = takeTurn1 boards draws 1

part2 :: ([Int], [Board]) -> Int
part2 (draws, boards) = takeTurn2 boards draws 1

takeTurn1 :: [Board] -> [Int] -> Int -> Int
takeTurn1 boards draws turn = case foldl1 (<|>) $ map (`boardScore` take turn draws) boards of
  Just v -> v
  Nothing -> takeTurn1 boards draws (turn + 1)

takeTurn2 :: [Board] -> [Int] -> Int -> Int
takeTurn2 [board] draws turn = fromJust $ boardScore board (take turn draws)
takeTurn2 boards draws t = takeTurn2 (filter (\b -> not (winningBoard b (take t draws))) boards) draws (t + 1)

boardScore :: Board -> [Int] -> Maybe Int
boardScore board draws = if winningBoard board draws then Just (last draws * sum (unmarkedNumbers board draws)) else Nothing

unmarkedNumbers :: Board -> [Int] -> [Int]
unmarkedNumbers board draws = concatMap (filter (`notElem` draws)) board

winningBoard :: Board -> [Int] -> Bool
winningBoard board draws = winningRows board draws || winningColumns board draws

winningColumns :: Board -> [Int] -> Bool
winningColumns = winningRows . transpose

winningRows :: Board -> [Int] -> Bool
winningRows board draws = any (`winningRow` draws) board

winningRow :: [Int] -> [Int] -> Bool
winningRow [] _ = True
winningRow (x : xs) draws = x `elem` draws && winningRow xs draws
