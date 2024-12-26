module Day22 where

import Data.Bits (shiftR, xor, (.&.), shiftL)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Prelude hiding ((>>), (^))

solveDay22 :: IO ()
solveDay22 = do
  example <- readFile "input/day22_example.txt"
  example2 <- readFile "input/day22_example2.txt"
  input <- readFile "input/day22_input.txt"

  putStrLn "\n\nDay 22\n---\nPart 1"
  print $ part1 $ parseInput example
  print $ part1 $ parseInput input
  putStrLn "\nPart 2"
  print $ part2 $ parseInput example2
  print $ part2 $ parseInput input

parseInput :: String -> [Int]
parseInput = map read . lines

part1 :: [Int] -> Int
part1 ns = sum $ iterate (map step) ns !! 2000

part2 :: [Int] -> Int
part2 ns =
  let secrets = map (take 2001 . iterate step) ns
      prices = map (map lastDigit) secrets
      priceChanges = map changes prices
      priceLists = map (reverse . drop 4) prices
      priceChangeLists = map reverse priceChanges
      sequenceMaps = zipWith (\p c -> createSequenceMap p c Map.empty) priceLists priceChangeLists
   in maximum . map snd $ Map.toList (zipMaps sequenceMaps)

step :: Int -> Int
step a =
  let b = (a << 6) ^ a & 16777215
      c = (b >> 5) ^ b
   in (c << 11) ^ c & 16777215

zipMaps :: [Map.Map [Int] Int] -> Map.Map [Int] Int
zipMaps ms = Map.fromList [(k, sum vs) | k <- keys, let vs = map (?! k) ms]
  where
    keys = concatMap Map.keys ms

createSequenceMap :: [Int] -> [Int] -> Map.Map [Int] Int -> Map.Map [Int] Int
createSequenceMap [] _ sequenceMap = sequenceMap
createSequenceMap (p : prices) cs sequenceMap = createSequenceMap prices (drop 1 cs) sequenceMap'
  where
    sequenceMap' = Map.insert (take 4 cs) p sequenceMap

changes :: [Int] -> [Int]
changes ns = zipWith (-) (tail ns) ns

lastDigit :: Int -> Int
lastDigit n = mod n 10

(?!) :: Map.Map [Int] Int -> [Int] -> Int
(?!) m key = fromMaybe 0 (Map.lookup key m)

(>>) :: Int -> Int -> Int
(>>) = shiftR

(<<) :: Int -> Int -> Int
(<<) = shiftL

(^) :: Int -> Int -> Int
(^) = xor

(&) :: Int -> Int -> Int
(&) = (.&.)
