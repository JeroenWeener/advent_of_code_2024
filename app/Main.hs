module Main where

import Day01 (solveDay01)
import Day02 (solveDay02)
import Day03 (solveDay03)
import Day04 (solveDay04)
import Day05 (solveDay05)
import Day06 (solveDay06)
import Day07 (solveDay07)
import Day08 (solveDay08)
import Day09 (solveDay09)
import Day10 (solveDay10)
import Day11 (solveDay11)
import Day12 (solveDay12)
import Day13 (solveDay13)
import Day14 (solveDay14)
import Day15 (solveDay15)
import Day16 (solveDay16)
import Day17 (solveDay17)
import Day18 (solveDay18)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

solutions :: [IO ()]
solutions =
  [ solveDay01,
    solveDay02,
    solveDay03,
    solveDay04,
    solveDay05,
    solveDay06,
    solveDay07,
    solveDay08,
    solveDay09,
    solveDay10,
    solveDay11,
    solveDay12,
    solveDay13,
    solveDay14,
    solveDay15,
    solveDay16,
    solveDay17,
    solveDay18
  ]

main :: IO ()
main = do
  putStr "Enter day to solve (or 'all' to run all): "
  hFlush stdout
  day <- getLine
  case day of
    "all" -> sequence_ solutions
    _ -> case readMaybe day :: Maybe Int of
      Just n | n >= 1 && n <= length solutions -> solutions !! (n - 1)
      _ -> putStrLn "Invalid day."
