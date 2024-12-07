module Main where

import Day01 (solveDay01)
import Day02 (solveDay02)
import Day03 (solveDay03)
import Day04 (solveDay04)
import Day05 (solveDay05)
import Day06 (solveDay06)
import Day07 (solveDay07)
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
    solveDay07
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
