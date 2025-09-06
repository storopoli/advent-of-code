module Day12 (part1) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text as T
import Data.Text qualified as Text
import Data.Text.IO as TIO
import Util (withInput)

type Point = (Int, Int)

type Grid = Map Point Char

parseInput :: Text -> Grid
parseInput input =
    Map.fromList
        [ ((row, col), c)
        | (row, line) <- Prelude.zip [0 ..] (T.unpack <$> T.lines input)
        , (col, c) <- Prelude.zip [0 ..] line
        ]

-- Get adjacent points
neighbors :: Point -> [Point]
neighbors (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

-- Find all points in a region starting from a point
findRegion :: Grid -> Point -> Char -> Set Point
findRegion grid start plant = go Set.empty [start]
  where
    go visited [] = visited
    go visited (p : ps)
        | Set.member p visited = go visited ps
        | Map.lookup p grid /= Just plant = go visited ps
        | otherwise =
            go
                (Set.insert p visited)
                (Prelude.filter (`Map.member` grid) (neighbors p) ++ ps)

-- Calculate region perimeter
calculatePerimeter :: Grid -> Set Point -> Int
calculatePerimeter grid region =
    sum
        [ 1 | p <- Set.toList region, n <- neighbors p, Map.lookup n grid /= Map.lookup p grid || not (Map.member n grid)
        ]

-- Find all regions in the grid
findAllRegions :: Grid -> [Set Point]
findAllRegions grid = go [] (Map.keys grid)
  where
    go _ [] = []
    go visited (p : ps)
        | Set.member p (mconcat visited) = go visited ps
        | otherwise = case Map.lookup p grid of
            Just plant ->
                let region = findRegion grid p plant
                 in region : go (region : visited) ps
            Nothing -> go visited ps

-- Calculate price for a single region
calculateRegionPrice :: Grid -> Set Point -> Int
calculateRegionPrice grid region =
    Set.size region * calculatePerimeter grid region

part1 :: String -> IO ()
part1 = withInput $ \inputStr -> do
    let input = T.lines (T.pack inputStr)
    let grid = parseInput (Text.unlines input)
        regions = findAllRegions grid
    print $ sum $ Prelude.map (calculateRegionPrice grid) regions
