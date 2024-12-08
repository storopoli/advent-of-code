{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (nub, tails)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

-- Part 1

type Position = (Int, Int)

type Station = (Char, Position)

type StationMap = Map Char [Position]

type Area = (Int, Int)

-- Check if a position is within the bounds of the area (inclusive)
inBounds :: Area -> Position -> Bool
inBounds (maxX, maxY) (x, y) = (x >= 0 && x <= maxX) && (y >= 0 && y <= maxY)

-- Check if a position is out of bounds of the area
outOfBonds :: Position -> Area -> Bool
outOfBonds pos area = not (inBounds area pos)

-- Takes a line of input and returns a List of (Char, Position)
-- Walks char by char until it finds something that is not a `.`,
-- then returns the char and the position of that char.
parseLine :: Int -> T.Text -> [Station]
parseLine y txt = go 0 (T.unpack txt)
  where
    go _ "" = []
    go x (c : cs) = case c of
      '.' -> go (x + 1) cs
      _ -> (c, (x, y)) : go (x + 1) cs

-- Takes a list of Station and returns a Map Char [Position]
-- It acumulates the Station Positions in a Map.
-- For example, if the input is [('A', (0, 0)), ('A', (1, 0)), ('B', (2, 0))]
-- it will return a Map where 'A' points to [(0, 0), (1, 0)] and 'B' points to [(2, 0)]
stationsToMap :: [Station] -> StationMap
stationsToMap = foldr f Map.empty
  where
    f (c, p) = Map.insertWith (++) c [p]

distance :: Position -> Position -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

-- Takes two positions an Area and returns all possible inline positions
-- from the line that is formed by the two positions, within bounds of the Area.
inLine :: Position -> Position -> Area -> [Position]
inLine (x1, y1) (x2, y2) (maxX, maxY) =
  let dx = x2 - x1
      dy = y2 - y1

      generatePoints k =
        let x = x1 + k * dx
            y = y1 + k * dy
         in (x, y)

      maxSteps
        | dx == 0 && dy == 0 = 0
        | dx == 0 = (maxY - y1) `div` abs dy
        | otherwise = (maxX - x1) `div` abs dx

      points = map generatePoints [0 .. maxSteps]
      inBounds = filter (not . (`outOfBonds` (maxX, maxY)))
   in inBounds points

-- Generates a list of all antinode positions for a given frequency within the bounds of an Area
antinodeList :: Char -> Area -> StationMap -> [Position]
antinodeList c area stations = case Map.lookup c stations of
  Nothing -> []
  Just positions -> concatMap (findAntinodes positions area) positions

-- For each pair of positions, find antinodes within the bounds of the Area
findAntinodes :: [Position] -> Area -> Position -> [Position]
findAntinodes positions area p1 =
  concatMap (generateAntinodes p1 area) positions

-- Generate antinode positions for a specific pair of positions (p1, p2)
generateAntinodes :: Position -> Area -> Position -> [Position]
generateAntinodes p1 area p2
  | p1 == p2 = []
  | otherwise =
      let (x1, y1) = p1
          (x2, y2) = p2
          dx = x2 - x1
          dy = y2 - y1
          antinode1 = (x1 - dx, y1 - dy) -- Extend the line segment in the opposite direction of p2
          antinode2 = (x2 + dx, y2 + dy) -- Extend the line segment beyond p2
       in filter (inBounds area) [antinode1, antinode2]

part1 :: IO ()
part1 = do
  input <- T.lines <$> TIO.readFile "input.txt"
  let width = T.length (head input)
  let height = length input
  let size = (width - 1, height - 1)
  let stations = stationsToMap (concat (zipWith parseLine [0 ..] input))
  let antinodes = concatMap (\c -> antinodeList c size stations) (Map.keys stations)
  let uniqueAntinodes = nub antinodes
  print (length uniqueAntinodes)

-- Part 2

antinodeList2 :: Char -> Area -> StationMap -> [Position]
antinodeList2 c area stations = case Map.lookup c stations of
  Nothing -> []
  Just positions ->
    let pairs = [(p1, p2) | (p1 : rest) <- tails positions, p2 <- rest]
        linesFromPairs = concatMap (linePointsFromPair area) pairs
     in linesFromPairs

linePointsFromPair :: Area -> (Position, Position) -> [Position]
linePointsFromPair area (p1@(x1, y1), p2@(x2, y2))
  | p1 == p2 = [] -- no line from identical points
  | otherwise =
      let dx = x2 - x1
          dy = y2 - y1
          g = gcd dx dy
          sx = dx `div` g
          sy = dy `div` g
          forward = generateInDirection p1 (sx, sy) area
          backward = generateInDirection p1 (-sx, -sy) area
       in nub (p1 : forward ++ backward) -- Include p1 itself

generateInDirection :: Position -> (Int, Int) -> Area -> [Position]
generateInDirection (x, y) (dx, dy) area =
  takeWhile (inBounds area) $ tail $ iterate (\(x', y') -> (x' + dx, y' + dy)) (x, y)

part2 :: IO ()
part2 = do
  input <- T.lines <$> TIO.readFile "input.txt"
  let width = T.length (head input)
  let height = length input
  let size = (width - 1, height - 1)
  let stations = stationsToMap (concat (zipWith parseLine [0 ..] input))
  let antinodes = concatMap (\c -> antinodeList2 c size stations) (Map.keys stations)
  let uniqueAntinodes = nub antinodes
  print (length uniqueAntinodes)

main :: IO ()
-- main = part1
main = part2
