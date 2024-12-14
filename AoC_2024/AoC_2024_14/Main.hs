{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

-- | Position of the Robot in x and y directions.
--
-- - Positive x is right, negative x is left.
-- - Positive y is down, negative y is up.
data Position = Position
  { x :: Int,
    y :: Int
  }
  deriving (Show)

-- | Velocity of the Robot in both x and y directions in coordinates per second.
type Velocity = Position

-- | Robot
type Robot = (Position, Velocity)

-- | Grid of Robots tiles.
--
-- The first coordinate is the width (X-axis)
-- and the second coordinate is the height (Y-axis).
--
-- - 11x 7 for `test_input.txt`
-- - 101x103 for `input.txt`
type Grid = (Int, Int)

-- | Quadrant of the grid.
type Quadrant = (Grid, Grid, Grid, Grid)

-- | Breaks a `Grid` into four `Quadrant`s by dividing width and height into equidistant parts.
--
-- The middle line is discarded, splitting the grid into four separate sections:
--
-- - Northwest (top-left)
-- - Northeast (top-right)
-- - Southwest (bottom-left)
-- - Southeast (bottom-right)
breakGrid :: Grid -> Quadrant
breakGrid (width, height) =
  let halfWidth = width `div` 2
      halfHeight = height `div` 2
      nw = (halfWidth, halfHeight)
      ne = (width - halfWidth, halfHeight)
      sw = (halfWidth, height - halfHeight)
      se = (width - halfWidth, height - halfHeight)
   in (nw, ne, sw, se)

-- | Teleports a Robot if movement out of bounds is detected.
teleport :: Grid -> Position -> Position
teleport (width, height) (Position x y) =
  Position (x `mod` width) (y `mod` height)

-- | Simulates a `Grid` for a given Robot `Position` and `Velocity`.
--
-- `teleport` is used to wrap around the edges of the grid.
simulate :: Grid -> Position -> Velocity -> Position
simulate (width, height) (Position x y) (Position vx vy) =
  let x' = x + vx
      y' = y + vy
   in if x' < 0 || x' >= width || y' < 0 || y' >= height
        then teleport (width, height) (Position x' y')
        else Position x' y'

-- | Simulates a `Grid` for a list of `Robot`s for a given number of time steps.
simulateGrid :: Grid -> [Robot] -> Int -> [Robot]
simulateGrid grid robots steps =
  let newPositions = map (\(pos, vel) -> (simulate grid pos vel, vel)) robots
   in if steps == 0
        then robots
        else simulateGrid grid newPositions (steps - 1)

-- | Determines if a `Robot` is in a given `Quadrant`.
inQuadrant :: Grid -> Robot -> Bool
inQuadrant (width, height) (Position px py, _) =
  px < width && px > 0 && py < height && py > 0

-- | Counts the number of robots in each Quadrant and multiplies them together.
safetyFactor :: Grid -> [Robot] -> Int
safetyFactor (width, height) robots =
  let midX = width `div` 2
      midY = height `div` 2
      nwRobots = length [(pos, vel) | (pos@(Position px py), vel) <- robots, px < midX, py < midY]
      neRobots = length [(pos, vel) | (pos@(Position px py), vel) <- robots, px > midX, py < midY]
      swRobots = length [(pos, vel) | (pos@(Position px py), vel) <- robots, px < midX, py > midY]
      seRobots = length [(pos, vel) | (pos@(Position px py), vel) <- robots, px > midX, py > midY]
   in nwRobots * neRobots * swRobots * seRobots

-- | Parses a `Robot`'s `Position` and `Velocity` from a single line of text.
parsePosition :: Text -> Robot
parsePosition line =
  let [pos, vel] = T.splitOn " " line -- Split "p=0,4 v=3,-3" into ["p=0,4", "v=3,-3"]
      [_, posCoords] = T.splitOn "=" pos -- Split "p=0,4" into ["p", "0,4"]
      [_, velCoords] = T.splitOn "=" vel -- Split "v=3,-3" into ["v", "3,-3"]
      [x, y] = T.splitOn "," posCoords -- Split "0,4" into ["0", "4"]
      [vx, vy] = T.splitOn "," velCoords -- Split "3,-3" into ["3", "-3"]
      x' = read $ T.unpack x
      y' = read $ T.unpack y
      vx' = read $ T.unpack vx
      vy' = read $ T.unpack vy
   in (Position x' y', Position vx' vy')

part1 :: IO ()
part1 = do
  input <- T.lines <$> TIO.readFile "input.txt"
  let robots = map parsePosition input
  let timeSteps = 100
  let grid = (101, 103)
  let final_robots = simulateGrid grid robots timeSteps
  let n_robots = safetyFactor grid final_robots
  print n_robots

main :: IO ()
main = part1
