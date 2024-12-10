module Day09 where

import Data.Char (digitToInt)
import Data.List (findIndex)
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust)

solveDay09 :: IO ()
solveDay09 = do
  example <- readFile "input/day09_example.txt"
  input <- readFile "input/day09_input.txt"

  putStrLn "\n\nDay 09\n---\nPart 1"
  print $ part1 $ parseInput1 example
  print $ part1 $ parseInput1 input
  putStrLn "\nPart 2"
  print $ part2 $ parseInput2 example
  print $ part2 $ parseInput2 input

-- Part 1 --

type File1 = (Int, Int)

type Space1 = Int

parseInput1 :: String -> ([File1], [Space1])
parseInput1 input = (fs, ss)
  where
    ns = map digitToInt input
    fs = parseFiles1 ns 0
    ss = 0 : parseSpaces1 ns

parseFiles1 :: [Int] -> Int -> [File1]
parseFiles1 [n] index = [(n, index)]
parseFiles1 (n1 : _ : ns) index = (n1, index) : parseFiles1 ns (index + 1)
parseFiles1 _ _ = []

parseSpaces1 :: [Int] -> [Space1]
parseSpaces1 (_ : n2 : ns) = n2 : parseSpaces1 ns
parseSpaces1 _ = []

part1 :: ([File1], [Space1]) -> Int
part1 input = (sum . zipWith (*) [0 ..]) (uncurry moveFiles1 input)

moveFiles1 :: [File1] -> [Space1] -> [Int]
moveFiles1 fs ss
  | null fs = []
  | null ss = fileBundle ++ moveFiles1 (tail fs) []
  | length fs == 1 = fileBundle
  | s == 0 = fileBundle ++ moveFiles1 (tail fs) (tail ss)
  | size == 0 = moveFiles1 (init fs) ss
  | otherwise = fileId : moveFiles1 fs' ((s - 1) : tail ss)
  where
    s = head ss
    f = head fs
    (size, fileId) = last fs
    fs' = init fs ++ [(size - 1, fileId)]
    fileBundle = uncurry replicate f

-- Part 2 --

type File2 = (Int, (Int, Int))

type FileMap2 = Map.Map Int (Int, Int)

type Space2 = (Int, Int)

parseInput2 :: String -> (FileMap2, [Space2])
parseInput2 input = (Map.fromList fs, ss)
  where
    ns = map digitToInt input
    fs = parseFiles2 ns 0 0
    ss = parseSpaces2 (tail ns) ((snd . snd . head) fs)

parseFiles2 :: [Int] -> Int -> Int -> [File2]
parseFiles2 [n] index fileId = [(fileId, (index, n))]
parseFiles2 (n1 : n2 : ns) index fileId = (fileId, (index, n1)) : parseFiles2 ns (index + n1 + n2) (fileId + 1)
parseFiles2 _ _ _ = []

parseSpaces2 :: [Int] -> Int -> [Space2]
parseSpaces2 (n1 : n2 : ns) index = (index, n1) : parseSpaces2 ns (index + n1 + n2)
parseSpaces2 _ _ = []

part2 :: (FileMap2, [Space2]) -> Int
part2 input@(_, ss) = sum . map score2 . Map.toList . fst $ moveFiles2 startFileId input
  where
    startFileId = length ss

moveFiles2 :: Int -> (FileMap2, [Space2]) -> (FileMap2, [Space2])
moveFiles2 (-1) disk = disk
moveFiles2 fileId disk = moveFiles2 (fileId - 1) (moveFile2 fileId disk)

moveFile2 :: Int -> (FileMap2, [Space2]) -> (FileMap2, [Space2])
moveFile2 fileId (fs, ss)
  | isJust emptySpaceIndex' && start1 > start2 = (fs', ss')
  | otherwise = (fs, ss)
  where
    (start1, size1) = fs ! fileId
    emptySpaceIndex' = findIndex (\(_, l) -> l >= size1) ss
    emptySpaceIndex = fromJust emptySpaceIndex'
    (start2, size2) = ss !! emptySpaceIndex
    shrunkenSpace = (start2 + size1, size2 - size1)
    fs' = Map.insert fileId (start2, size1) fs
    neighborSpaceIndex = fromJust $ findIndex (\(s, l) -> s + l == start1) ss
    (start3, size3) = ss !! neighborSpaceIndex
    expandedSpace = (start3, size3 + size1)
    ss' = replaceAt expandedSpace neighborSpaceIndex (replaceAt shrunkenSpace emptySpaceIndex ss)

score2 :: File2 -> Int
score2 (_, (_, 0)) = 0
score2 (fileId, (start, size)) = fileId * (start + size - 1) + score2 (fileId, (start, size - 1))

replaceAt :: a -> Int -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt e 0 (_ : ns) = e : ns
replaceAt e i (n : ns) = n : replaceAt e (i - 1) ns
