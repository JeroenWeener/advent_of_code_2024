module Day15 where

import Data.List (intersect, sort, sortOn, (\\))

type C = (Int, Int)

type Walls = [C]

type Boxes = [C]

type Robot = C

type Instructions = String

solveDay15 :: IO ()
solveDay15 = do
  example <- readFile "input/day15_example.txt"
  example2 <- readFile "input/day15_example2.txt"
  input <- readFile "input/day15_input.txt"

  putStrLn "\n\nDay 15\n---\nPart 1"
  print $ part1 $ parseInput example
  print $ part1 $ parseInput example2
  print $ part1 $ parseInput input

  putStrLn "\nPart 2"
  print $ part2 $ parseInput example2
  print $ part2 $ parseInput input

parseInput :: String -> (Walls, Boxes, Robot, Instructions)
parseInput input = (walls, boxes, robot, concat instructions)
  where
    (grid, instructions) = break null (lines input)
    walls = parseWalls grid
    boxes = parseBoxes grid
    robot = parseRobot grid

parseEntity :: Char -> [String] -> [C]
parseEntity c ls = [(y, x) | y <- [0 .. length ls - 1], x <- [0 .. length ls - 1], ls !! y !! x == c]

parseWalls :: [String] -> Walls
parseWalls = parseEntity '#'

parseBoxes :: [String] -> Boxes
parseBoxes = parseEntity 'O'

parseRobot :: [String] -> Robot
parseRobot = head . parseEntity '@'

(+:) :: C -> C -> C
(+:) (a, b) (c, d) = (a + c, b + d)

(-:) :: C -> C -> C
(-:) (a, b) (c, d) = (a - c, b - d)

direction :: Char -> C
direction '^' = (-1, 0)
direction '>' = (0, 1)
direction 'v' = (1, 0)
direction _ = (0, -1)

-- Part 1 --

part1 :: (Walls, Boxes, Robot, Instructions) -> Int
part1 input = sum $ map (\(y, x) -> 100 * y + x) bs
  where
    (_, bs, _, _) = moveRobot1 input

moveRobot1 :: (Walls, Boxes, Robot, Instructions) -> (Walls, Boxes, Robot, Instructions)
moveRobot1 (walls, boxes, robot, []) = (walls, boxes, robot, [])
moveRobot1 (walls, boxes, robot, i : is) =
  if canMove
    then moveRobot1 (walls, boxes' \\ [robot'], robot', is)
    else moveRobot1 (walls, boxes \\ [robot], robot, is)
  where
    boxes' = moveBox (walls, boxes, robot, i)
    canMove = sort (robot : boxes) /= sort boxes'
    robot' = robot +: direction i

moveBox :: (Walls, Boxes, C, Char) -> Boxes
moveBox (ws, bs, b, i)
  | b' `elem` ws = b : bs
  | b' `elem` bs =
      if canMove
        then b' : bs'
        else b : bs
  | otherwise = b' : bs
  where
    b' = b +: direction i
    bs' = moveBox (ws, bs \\ [b'], b', i)
    canMove = sort bs /= sort bs'

-- Part 2 --

part2 :: (Walls, Boxes, Robot, Instructions) -> Int
part2 (walls, boxes, robot, instructions) = sum $ map (\(y, x) -> 100 * y + x) bs
  where
    walls' = map widen walls
    boxes' = map widen boxes
    robot' = widen robot
    (_, bs, _, _) = moveRobot2 (walls', boxes', robot', instructions)

widen :: C -> C
widen (y, x) = (y, 2 * x)

moveRobot2 :: (Walls, Boxes, Robot, Instructions) -> (Walls, Boxes, Robot, Instructions)
moveRobot2 (ws, bs, r, []) = (ws, bs, r, [])
moveRobot2 (ws, bs, r, is)
  | head is == '<' = moveRobotLeft (ws, bs, r, is)
  | head is == '>' = moveRobotRight (ws, bs, r, is)
  | otherwise = moveRobotVertical (ws, bs, r, is)

moveRobotLeft :: (Walls, Boxes, Robot, Instructions) -> (Walls, Boxes, Robot, Instructions)
moveRobotLeft (walls, boxes, robot, []) = (walls, boxes, robot, [])
moveRobotLeft (walls, boxes, robot, i : is)
  | (robot' +: direction i) `elem` walls = moveRobot2 (walls, boxes, robot, is)
  | (robot' +: direction i) `elem` boxes =
      if canMove
        then moveRobot2 (walls, boxes' \\ [robot'], robot', is)
        else moveRobot2 (walls, boxes, robot, is)
  | otherwise = moveRobot2 (walls, boxes, robot', is)
  where
    robot' = robot +: direction i
    boxes' = moveBoxHorizontal (walls, boxes, robot, i)
    canMove = sort (robot : boxes) /= sort boxes'

moveRobotRight :: (Walls, Boxes, Robot, Instructions) -> (Walls, Boxes, Robot, Instructions)
moveRobotRight (walls, boxes, robot, []) = (walls, boxes, robot, [])
moveRobotRight (walls, boxes, robot, i : is)
  | robot' `elem` walls = moveRobot2 (walls, boxes, robot, is)
  | robot' `elem` boxes =
      if canMove
        then moveRobot2 (walls, boxes' \\ [robot], robot', is)
        else moveRobot2 (walls, boxes, robot, is)
  | otherwise = moveRobot2 (walls, boxes, robot', is)
  where
    robot' = robot +: direction i
    boxes' = moveBoxHorizontal (walls, boxes, robot -: direction i, i)
    canMove = sort (robot -: direction i : boxes) /= sort boxes'

moveRobotVertical :: (Walls, Boxes, Robot, Instructions) -> (Walls, Boxes, Robot, Instructions)
moveRobotVertical (walls, boxes, robot, []) = (walls, boxes, robot, [])
moveRobotVertical (walls, boxes, robot, i : is)
  | robot' `elem` walls || robot' +: (0, -1) `elem` walls = moveRobot2 (walls, boxes, robot, is)
  | robot' `elem` boxes =
      if canMove
        then moveRobot2 (walls, boxes' \\ [robot'], robot', is)
        else moveRobot2 (walls, boxes, robot, is)
  | robot' +: (0, -1) `elem` boxes =
      if canMove'
        then moveRobot2 (walls, boxes'' \\ [robot' +: (0, -1)], robot', is)
        else moveRobot2 (walls, boxes, robot, is)
  | otherwise = moveRobot2 (walls, boxes, robot', is)
  where
    robot' = robot +: direction i

    boxes' = moveBoxVertical (walls, boxes, robot', i)
    canMove = sort (robot' : boxes) /= sort boxes'

    boxes'' = moveBoxVertical (walls, boxes, robot' +: (0, -1), i)
    canMove' = sort (robot' +: (0, -1) : boxes) /= sort boxes''

moveBoxHorizontal :: (Walls, Boxes, C, Char) -> Boxes
moveBoxHorizontal (ws, bs, b, i)
  | (b' +: direction i) `elem` ws = b : bs
  | (b' +: direction i) `elem` bs =
      if canMove
        then b' : bs'
        else b : bs
  | otherwise = b' : bs
  where
    b' = b +: direction i
    bs' = moveBoxHorizontal (ws, bs \\ [b' +: direction i], b' +: direction i, i)
    canMove = sort bs /= sort bs'

moveBoxVertical :: (Walls, Boxes, C, Char) -> Boxes
moveBoxVertical (ws, bs, b, i)
  | b' `elem` ws || b' +: (0, -1) `elem` ws || b' +: (0, 1) `elem` ws = b : bs
  | touchingBoxes == 1 =
      if canMove
        then b' : bs'
        else b : bs
  | touchingBoxes == 2 =
      if canMove'
        then b' : merged
        else b : bs
  | otherwise = b' : bs
  where
    b' = b +: direction i
    adjacentRow =
      map
        (+: direction i)
        [ b +: (0, -1),
          b +: (0, 0),
          b +: (0, 1)
        ]
    touchingBoxes = (length . filter (`elem` bs)) adjacentRow

    -- Touching 1 box
    bs' = concat [moveBoxVertical (ws, bs \\ [adjacentBox], adjacentBox, i) | adjacentBox <- adjacentRow, adjacentBox `elem` bs]
    canMove = sort bs /= sort bs'

    -- Touching 2 boxes
    bs1' = moveBoxVertical (ws, bs \\ [b' +: (0, -1)], b' +: (0, -1), i)
    bs2' = moveBoxVertical (ws, bs \\ [b' +: (0, 1)], b' +: (0, 1), i)
    canMove' = sort bs /= sort bs1' && sort bs /= sort bs2'
    merged = mergeChangesVertical bs1' bs2' i

mergeChangesVertical :: Boxes -> Boxes -> Char -> Boxes
mergeChangesVertical bs1 bs2 i = agreed ++ bestCandidates
  where
    agreed = bs1 `intersect` bs2
    as = bs1 \\ agreed
    bs = bs2 \\ agreed

    as' = sortOn snd $ sortOn fst as
    bs' = sortOn snd $ sortOn fst bs
    bestCandidates = zipWith (\a b -> if i == '^' then min a b else max a b) as' bs'
