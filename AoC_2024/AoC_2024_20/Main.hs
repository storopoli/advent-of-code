{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Ord (Down (..))
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

type Pos = (Int, Int)

type Grid = M.Map Pos Char

type Dist = M.Map Pos Int

-- Parse input into grid and find start/end positions
parseInput :: T.Text -> (Grid, Pos, Pos)
parseInput input = (grid, start, end)
  where
    grid =
      M.fromList
        [ ((x, y), c)
          | (y, line) <- zip [0 ..] $ T.lines input,
            (x, c) <- zip [0 ..] $ T.unpack line
        ]
    start = head [(x, y) | ((x, y), c) <- M.toList grid, c == 'S']
    end = head [(x, y) | ((x, y), c) <- M.toList grid, c == 'E']

-- Get adjacent positions
neighbors :: Pos -> [Pos]
neighbors (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

-- Find distances from a starting position
findDistances :: Grid -> Pos -> Dist
findDistances grid start = go initialDist [start]
  where
    initialDist = M.singleton start 0

    go :: Dist -> [Pos] -> Dist
    go dist [] = dist
    go dist (pos : queue) =
      let curDist = dist M.! pos
          nextPoss =
            [ n | n <- neighbors pos, M.findWithDefault '#' n grid /= '#', M.findWithDefault maxBound n dist > curDist + 1
            ]
          newDist = foldr (\p m -> M.insert p (curDist + 1) m) dist nextPoss
       in go newDist (queue ++ nextPoss)

-- Find cheats by looking at wall crossings
findCheats :: Grid -> Pos -> Pos -> Int -> [(Int, Int)]
findCheats grid start end normalTime = do
  let fromStart = findDistances grid start
  let fromEnd = findDistances grid end

  -- Find all possible wall crossings
  let wallCrossings = do
        pos1 <- M.keys grid
        pos2 <- neighbors pos1
        guard $ M.findWithDefault '#' pos2 grid == '#'
        pos3 <- neighbors pos2
        guard $ M.findWithDefault '#' pos3 grid /= '#'

        let dist1 = M.findWithDefault maxBound pos1 fromStart
        let dist3 = M.findWithDefault maxBound pos3 fromEnd
        guard $ dist1 /= maxBound && dist3 /= maxBound

        let totalDist = dist1 + 2 + dist3 -- 2 steps to cross wall
        let saved = normalTime - totalDist
        guard $ saved >= 2 -- Must save some time
        return (saved, 1)

  M.toList $ M.fromListWith (+) wallCrossings
  where
    guard True = [()]
    guard False = []

part1 :: IO ()
part1 = do
  input <- TIO.readFile "input.txt"
  let (grid, start, end) = parseInput input
  let normalTime = findDistances grid start M.! end
  putStrLn $ "Normal path length: " ++ show normalTime

  let cheats = findCheats grid start end normalTime
  let validCheats = filter ((>= 100) . fst) cheats
  putStrLn $
    "Number of cheats saving ≥100 picoseconds: "
      ++ show (sum [count | (_, count) <- validCheats])

main :: IO ()
main = part1
