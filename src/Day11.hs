module Day11 where

import Data.Map ((!))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

type Cache = Map.Map Int (Map.Map Int Int)

solveDay11 :: IO ()
solveDay11 = do
  example <- readFile "input/day11_example.txt"
  input <- readFile "input/day11_input.txt"

  putStrLn "\n\nDay 11\n---\nPart 1"
  print $ part1 $ parseInput example
  print $ part1 $ parseInput input
  putStrLn "\nPart 2"
  print $ part2 $ parseInput example
  print $ part2 $ parseInput input

parseInput :: String -> [Int]
parseInput = map read . words

part1 :: [Int] -> Int
-- part1 input = length $ iterate (concatMap blink) input !! 25
part1 = sum . map (fst . blinkCached 25 Map.empty)

part2 :: [Int] -> Int
part2 = sum . map (fst . blinkCached 75 Map.empty)

blink :: Int -> [Int]
blink stone
  | stone == 0 = [1]
  | even . length $ show stone = [read l, read r]
  | otherwise = [stone * 2024]
  where
    s = show stone
    (l, r) = splitAt (length s `div` 2) s

cacheContains :: Int -> Int -> Cache -> Bool
cacheContains depth stone cache =
  Map.member stone cache
    && Map.member depth (cache ! stone)

cacheInsert :: Int -> Int -> Int -> Cache -> Cache
cacheInsert depth stone size cache = cache'
  where
    subcache = fromMaybe Map.empty (Map.lookup stone cache)
    subcache' = Map.insert depth size subcache
    cache' = Map.insert stone subcache' cache

blinkCached :: Int -> Cache -> Int -> (Int, Cache)
blinkCached depth cache stone
  | depth == 0 = (1, cacheInsert 0 stone 1 cache)
  | cacheContains depth stone cache = (cache ! stone ! depth, cache)
  | otherwise = (totalSize, updatedCache')
  where
    (totalSize, updatedCache) =
      foldl
        ( \(subtotalSize, cache') substone ->
            let (subsize, cache'') = blinkCached (depth - 1) cache' substone
             in (subsize + subtotalSize, cache'')
        )
        (0, cache)
        (blink stone)
    updatedCache' = cacheInsert depth stone totalSize updatedCache
