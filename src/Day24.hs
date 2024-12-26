module Day24 where

import Data.Bits (xor, (.&.), (.|.))
import Data.Char (digitToInt)
import Data.List (find, intercalate, sort)
import Data.List.Split (splitOn)
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Maybe (fromJust)

type Gate = (String, String, String)

solveDay24 :: IO ()
solveDay24 = do
  example <- readFile "input/day24_example.txt"
  input <- readFile "input/day24_input.txt"

  putStrLn "\n\nDay 24\n---\nPart 1"
  print $ part1 $ parseInput example
  print $ part1 $ parseInput input

  putStrLn "\nPart 2"
  print $ part2 $ parseInput input

parseInput :: String -> (Map.Map String Int, Map.Map String Gate, [String])
parseInput input =
  let (sectionA, sectionB) = parseSections input
      inputsMap =
        Map.fromList
          [ (bit, value)
            | l <- lines sectionA,
              let bit = take 3 l,
              let value = digitToInt (last l)
          ]
      gates = lines sectionB
      gateMap = Map.fromList [parseGate gate | gate <- gates]
      zGates = parseZGates gates
   in (inputsMap, gateMap, zGates)

parseSections :: String -> (String, String)
parseSections input = case splitOn "\n\n" input of
  [sectionA, sectionB] -> (sectionA, sectionB)
  _ -> error ""

parseGate :: String -> (String, Gate)
parseGate line = case words line of
  [left, op, right, _, key] -> (key, (left, right, op))
  _ -> error ""

parseZGates :: [String] -> [String]
parseZGates ls = sort [zGate | l <- ls, let zGate = last (words l), head zGate == 'z']

part1 :: (Map.Map String Int, Map.Map String Gate, [String]) -> Int
part1 (inputsMap, gateMap, zGates) = produceNumber $ map (evaluate inputsMap gateMap) zGates

{-
It is likely that the part 2 algorithms are overfitted for my specific input,
and might not correctly handle all possible edge cases.
-}
part2 :: (Map.Map String Int, Map.Map String Gate, [String]) -> String
part2 (_, gateMap, _) = produceSequence $ checkCircuits gateMap 1 []

produceNumber :: [Int] -> Int
produceNumber [] = 0
produceNumber (n : ns) = n + 2 * produceNumber ns

produceSequence :: [String] -> String
produceSequence = intercalate "," . sort

evaluate :: Map.Map String Int -> Map.Map String Gate -> String -> Int
evaluate inputsMap gateMap variable
  | Map.member variable inputsMap = inputsMap ! variable
  | otherwise = operation op (evaluate inputsMap gateMap left) (evaluate inputsMap gateMap right)
  where
    (left, right, op) = gateMap ! variable

operation :: String -> (Int -> Int -> Int)
operation "AND" = (.&.)
operation "XOR" = xor
operation _ = (.|.)

checkCircuits :: Map.Map String Gate -> Int -> [String] -> [String]
checkCircuits _ 45 swapped = swapped
checkCircuits gateMap zGateNumber swapped =
  let expectedGates = expectedLeafs gateMap zGateNumber
      gate = "z" ++ pad zGateNumber
   in case findSwappedGates gateMap gate expectedGates of
        Just (left, right) ->
          let gateMap' = swapGates gateMap left right
           in checkCircuits gateMap' (zGateNumber + 1) (left : right : swapped)
        Nothing -> checkCircuits gateMap (zGateNumber + 1) swapped

swapGates :: Map.Map String Gate -> String -> String -> Map.Map String Gate
swapGates input a b = Map.insert b (input ! a) $ Map.insert a (input ! b) input

pad :: Int -> String
pad n
  | n < 10 = "0" ++ show n
  | otherwise = show n

firstJust :: [Maybe a] -> Maybe a
firstJust (Just x : _) = Just x
firstJust (Nothing : xs) = firstJust xs
firstJust [] = Nothing

expectedLeafs :: Map.Map String Gate -> Int -> [String]
expectedLeafs gateMap gateNumber =
  map (findLeaf gateMap) $
    (pad gateNumber, "XOR")
      : concat
        [ [(pad i, "AND"), (pad i, "XOR")]
          | i <- [gateNumber - 1, gateNumber - 2 .. 1]
        ]

findLeaf :: Map.Map String Gate -> (String, String) -> String
findLeaf gateMap (gateNumber, op1) =
  fst $
    fromJust $
      find (\(_, (gate, _, op2)) -> op1 == op2 && drop 1 gate == gateNumber) (Map.toList gateMap)

findSwappedGates :: Map.Map String Gate -> String -> [String] -> Maybe (String, String)
findSwappedGates _ _ [] = Nothing
findSwappedGates gateMap gate (e : expectedGates)
  | head gate == 'z' && op /= "XOR" = Just (gate, f gateMap e)
  | left == e = findSwappedGates gateMap right expectedGates
  | right == e = findSwappedGates gateMap left expectedGates
  | otherwise = Just (g gateMap (head expectedGates) left right, e)
  where
    (left, right, op) = gateMap ! gate

f :: Map.Map String Gate -> String -> String
f gateMap gate =
  fst $
    fromJust $
      find (\(_, (left, right, op)) -> (left == gate || right == gate) && op == "XOR") (Map.toList gateMap)

g :: Map.Map String Gate -> String -> String -> String -> String
g gateMap gate left right = if l == gate || r == gate then right else left
  where
    (l, r, _) = gateMap ! left
