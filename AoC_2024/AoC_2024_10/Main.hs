{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (guard)
import Control.Monad.State (State, evalState, gets, modify)
import Data.Array (Array, assocs, bounds, listArray, (!))
import Data.Char (digitToInt)
import Data.List (nub)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

-- | Type alias for a grid of heights.
type Grid = Array (Int, Int) Int

-- | Load the grid from input.
parseGrid :: [Text] -> Grid
parseGrid lines = listArray ((0, 0), (rows - 1, cols - 1)) values
  where
    rows = length lines
    cols = T.length (head lines)
    values = concatMap (map digitToInt . T.unpack) lines

-- | Find all trailheads (positions with height 0) in the grid.
findTrailheads :: Grid -> [(Int, Int)]
findTrailheads grid = [pos | (pos, h) <- assocs grid, h == 0]

-- | Determine if a position is in bounds of the grid.
inBounds :: Grid -> (Int, Int) -> Bool
inBounds grid (r, c) = bounds grid `contains` (r, c)
  where
    ((rMin, cMin), (rMax, cMax)) = bounds grid
    contains ((rMin', cMin'), (rMax', cMax')) (r', c') =
      (rMin' <= r' && r' <= rMax') && (cMin' <= c' && c' <= cMax')

-- | Get valid neighbors (up, down, left, right) of a position in the grid.
neighbors :: Grid -> (Int, Int) -> [(Int, Int)]
neighbors grid (r, c) = do
  (dr, dc) <- [(-1, 0), (1, 0), (0, -1), (0, 1)]
  let newPos = (r + dr, c + dc)
  guard (inBounds grid newPos)
  pure newPos

-- Part 1

-- | DFS to find all reachable `9`s starting from a trailhead.
findReachableNines :: Grid -> (Int, Int) -> Set (Int, Int)
findReachableNines grid start = dfs Set.empty [start]
  where
    dfs visited [] = visited
    dfs visited (p : ps)
      | p `Set.member` visited = dfs visited ps
      | otherwise =
          let currentHeight = grid ! p
              nextPositions = [n | n <- neighbors grid p, grid ! n == currentHeight + 1]
              newVisited = Set.insert p visited
           in dfs newVisited (nextPositions ++ ps)

-- | Calculate the score of a single trailhead by counting reachable `9`s.
trailheadScore :: Grid -> (Int, Int) -> Int
trailheadScore grid trailhead = length $ filter (\pos -> grid ! pos == 9) reachable
  where
    reachable = Set.toList $ findReachableNines grid trailhead

-- | Part 1: Calculate the sum of all trailhead scores.
part1 :: IO ()
part1 = do
  input <- T.lines <$> TIO.readFile "input.txt"
  let grid = parseGrid input
      trailheads = findTrailheads grid
      scores = map (trailheadScore grid) trailheads
      totalScore = sum scores
  print totalScore

-- Part 2

-- | Count distinct paths to a height of 9 using DFS with memoization.
countPaths :: Grid -> (Int, Int) -> State (Map (Int, Int) Int) Int
countPaths grid pos
  | grid ! pos == 9 = return 1
  | otherwise = do
      memo <- gets (Map.lookup pos)
      case memo of
        Just result -> return result
        Nothing -> do
          let currentHeight = grid ! pos
              nextPositions = [n | n <- neighbors grid pos, grid ! n == currentHeight + 1]
          result <- sum <$> mapM (countPaths grid) nextPositions
          modify (Map.insert pos result)
          return result

-- | Calculate the rating of a single trailhead by counting distinct hiking trails.
trailheadRating :: Grid -> (Int, Int) -> Int
trailheadRating grid trailhead = evalState (countPaths grid trailhead) Map.empty

-- | Solve Part 2: Calculate the sum of all trailhead ratings.
part2 :: IO ()
part2 = do
  input <- T.lines <$> TIO.readFile "input.txt"
  let grid = parseGrid input
      trailheads = findTrailheads grid
      ratings = map (trailheadRating grid) trailheads
      totalRating = sum ratings
  print totalRating

main :: IO ()
-- main = part1
main = part2
