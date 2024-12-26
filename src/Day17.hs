module Day17 where

import Data.Bits (shiftR, xor, (.&.), shiftL)
import Data.Char (digitToInt)
import Data.List (elemIndices)
import Data.Maybe (fromJust)
import Prelude hiding ((>>), (^))

type Registers = (Int, Int, Int)

type Operations = [Int]

solveDay17 :: IO ()
solveDay17 = do
  example <- readFile "input/day17_example.txt"
  example2 <- readFile "input/day17_example2.txt"
  input <- readFile "input/day17_input.txt"

  putStrLn "\n\nDay 17\n---\nPart 1"
  print $ part1 $ parseInput example
  print $ part1 $ parseInput input

  putStrLn "\nPart 2"
  print $ part2 $ parseInput example2
  print $ part2 $ parseInput input

parseInput :: String -> (Registers, Operations)
parseInput input =
  let (registerString, programString) = break null (lines input)
      registers = parseRegisters registerString
      program = parseProgram (last programString)
   in (registers, program)

parseRegisters :: [String] -> Registers
parseRegisters ls = case map (read . drop 12) ls of
  [a, b, c] -> (a, b, c)
  _ -> error ""

parseProgram :: String -> Operations
parseProgram = map digitToInt . everySecond . drop 9

everySecond :: String -> String
everySecond (e : _ : es) = e : everySecond es
everySecond es = es

part1 :: (Registers, Operations) -> [Int]
part1 (registers, ops) = run (registers, ops, 0, [])

{-
Assume that the program:
  - Contains a single `jnz` instruction.
    - As the last instruction, jumping back to 0 (3,0).
  - Contains a single `out` instruction.
    - As the second last instruction (5,_).
  - Contains a single `adv` instruction.
    - With literal operand 3 (0,3).
-}
part2 :: (Registers, Operations) -> Int
part2 (_, ops) =
  let inToOutMap = inToOut ops
      outToInMap = outToIn inToOutMap
      optionsList = optionsPerOutput ops outToInMap
      picked = selectOptions [] (reverse optionsList)
      stitched = stitchOptions (fromJust picked)
   in stitched

run :: (Registers, Operations, Int, [Int]) -> [Int]
run (rs@(a, b, c), ops, ip, output)
  | ip >= length ops = output
  | opcode == 0 = run ((a >> combo rs operand, b, c), ops, ip + 2, output)
  | opcode == 1 = run ((a, b ^ operand, c), ops, ip + 2, output)
  | opcode == 2 = run ((a, combo rs operand & 7, c), ops, ip + 2, output)
  | opcode == 3 && a == 0 = run (rs, ops, ip + 2, output)
  | opcode == 3 = run (rs, ops, operand, output)
  | opcode == 4 = run ((a, b ^ c, c), ops, ip + 2, output)
  | opcode == 5 = run (rs, ops, ip + 2, output ++ [combo rs operand & 7])
  | opcode == 6 = run ((a, a >> combo rs operand, c), ops, ip + 2, output)
  | otherwise = run ((a, b, a >> combo rs operand), ops, ip + 2, output)
  where
    opcode = ops !! ip
    operand = ops !! (ip + 1)

combo :: Registers -> Int -> Int
combo (a, b, c) = ([0, 1, 2, 3, a, b, c] !!)

(>>) :: Int -> Int -> Int
(>>) = shiftR

(<<) :: Int -> Int -> Int
(<<) = shiftL

(^) :: Int -> Int -> Int
(^) = xor

(&) :: Int -> Int -> Int
(&) = (.&.)

-- Calculate 3-bit output of every 10-bit integer.
inToOut :: Operations -> [Int]
inToOut ops = map (\a -> head $ run ((a, 0, 0), ops, 0, [])) [0 .. 1023]

-- Create mapping of 3-bit integers to all their possible 10-bit integer inputs.
outToIn :: [Int] -> [[Int]]
outToIn inToOutMap = [inputs | i <- [0 .. 7], let inputs = i `elemIndices` inToOutMap]

-- List all possible 1-bit inputs per 3-bit output.
optionsPerOutput :: Operations -> [[Int]] -> [[Int]]
optionsPerOutput ops outToInMap = map (outToInMap !!) ops

-- Pick one option per output such that adjacent outputs have overlapping inputs.
selectOptions :: [Int] -> [[Int]] -> Maybe [Int]
selectOptions picked optionsList
  | length picked == length optionsList = Just picked
  | null picked = maybeFirst $ map (\option -> selectOptions [option] optionsList) (head optionsList)
  | null nextOptions = Nothing
  | otherwise = maybeFirst $ map (\option -> selectOptions (picked ++ [option]) optionsList) nextOptions
  where
    currentIndex = length picked
    prev = last picked
    nextOptions = filter (overlap prev) (optionsList !! currentIndex)

-- Combine all overlapping input to a single input.
stitchOptions :: [Int] -> Int
stitchOptions = foldl1 (\c n -> (c << 3) + (n & 7))

-- Determine whether two 10-bit integers overlap.
overlap :: Int -> Int -> Bool
overlap l r = (l & 127) == (r >> 3)

maybeFirst :: [Maybe a] -> Maybe a
maybeFirst [] = Nothing
maybeFirst (Just n : _) = Just n
maybeFirst (Nothing : ns) = maybeFirst ns
