{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

type Coord = (Int, Int)

type Grid = Set.Set Coord

parseCoord :: Text -> Coord
parseCoord line = case T.splitOn "," line of
  [x, y] -> (read (T.unpack x), read (T.unpack y))
  _ -> error "Invalid input format"

neighbors :: Grid -> Int -> Int -> Coord -> [Coord]
neighbors grid maxX maxY (x, y) =
  filter isValid [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
  where
    isValid (nx, ny) =
      nx >= 0
        && nx <= maxX
        && ny >= 0
        && ny <= maxY
        && not (Set.member (nx, ny) grid)

bfs :: Grid -> Int -> Int -> Coord -> Coord -> Maybe Int
bfs grid maxX maxY start end = go (Set.singleton start) [(start, 0)]
  where
    go visited [] = Nothing
    go visited ((pos, dist) : queue)
      | pos == end = Just dist
      | otherwise =
          let nextSteps = neighbors grid maxX maxY pos
              newSteps = filter (`Set.notMember` visited) nextSteps
              newVisited = foldr Set.insert visited newSteps
              newQueue = queue ++ map (,dist + 1) newSteps
           in go newVisited newQueue

part1 :: IO ()
part1 = do
  input <- T.lines <$> TIO.readFile "input.txt"
  let coords = map parseCoord input
      grid = Set.fromList (take 1024 coords) -- Take first 1024 bytes
      maxX = 70
      maxY = 70
      result = bfs grid maxX maxY (0, 0) (maxX, maxY)

  case result of
    Nothing -> putStrLn "No path found!"
    Just steps -> putStrLn $ "Minimum steps needed: " ++ show steps

-- Part 2

parseCoord2 :: Text -> String
parseCoord2 line = case T.splitOn "," line of
  [x, y] -> T.unpack x ++ "," ++ T.unpack y -- Keep original format for output
  _ -> error "Invalid input format"

part2 :: IO ()
part2 = do
  input <- T.lines <$> TIO.readFile "input.txt"
  let coords = map parseCoord input
      maxX = 70
      maxY = 70

      -- Try each prefix length until we find the blocking coordinate
      firstBlocking = head $ dropWhile hasPath [1 .. length coords]
      blockingCoord = coords !! (firstBlocking - 1)

      -- Helper function to check if a path exists with given number of bytes
      hasPath n =
        let grid = Set.fromList (take n coords)
         in case bfs grid maxX maxY (0, 0) (maxX, maxY) of
              Just _ -> True
              Nothing -> False

  -- Get the original string format of the blocking coordinate
  putStrLn $ parseCoord2 (input !! (firstBlocking - 1))

main :: IO ()
-- main = part1
main = part2
