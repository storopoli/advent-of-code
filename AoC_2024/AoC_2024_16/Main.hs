{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

-- Core Types
type Coordinate = (Int, Int)

type TileMap = M.Map Coordinate Tile

-- | Direction for reindeer movement
data Direction = North | South | East | West
  deriving (Show, Eq, Ord)

-- | Tile types in the map
data Tile = End | Floor | Start | Wall
  deriving (Show, Eq)

-- | State includes both position and direction
type State = (Coordinate, Direction)

-- | Parse a character into a tile
parseTile :: Char -> Tile
parseTile = \case
  '#' -> Wall
  '.' -> Floor
  'S' -> Start
  'E' -> End
  c -> error $ "Invalid tile: " <> show c

-- | Parse the map from text input
parseMap :: Text -> (TileMap, Coordinate, Coordinate)
parseMap input =
  let tiles = do
        (y, line) <- zip [0 ..] (T.lines input)
        (x, char) <- zip [0 ..] (T.unpack line)
        return ((x, y), parseTile char)
      tileMap = M.fromList tiles
      start = head [pos | (pos, tile) <- tiles, tile == Start]
      end = head [pos | (pos, tile) <- tiles, tile == End]
   in (tileMap, start, end)

-- | Get next possible moves from current state
getNextMoves :: TileMap -> State -> [(State, Int)]
getNextMoves map' (pos, dir) =
  forwardMove ++ turnMoves
  where
    forwardMove = case moveForward pos dir of
      newPos
        | Just tile <- M.lookup newPos map',
          tile /= Wall ->
            [((newPos, dir), 1)]
        | otherwise -> []
    turnMoves = case dir of
      North -> [((pos, East), 1000), ((pos, West), 1000)]
      South -> [((pos, East), 1000), ((pos, West), 1000)]
      East -> [((pos, North), 1000), ((pos, South), 1000)]
      West -> [((pos, North), 1000), ((pos, South), 1000)]

-- | Move one step forward in current direction
moveForward :: Coordinate -> Direction -> Coordinate
moveForward (x, y) = \case
  North -> (x, y - 1)
  South -> (x, y + 1)
  East -> (x + 1, y)
  West -> (x - 1, y)

-- | Find shortest path using Dijkstra's algorithm
findShortestPath :: TileMap -> Coordinate -> Coordinate -> Int
findShortestPath map' start end = dijkstra initialCosts initialVisited
  where
    initialCosts = M.singleton (start, East) 0
    initialVisited = S.empty

    dijkstra costs visited =
      if null unvisitedStates
        then
          if null endStateCosts
            then maxBound
            else minimum endStateCosts
        else dijkstra newCosts newVisited
      where
        -- Get all states we haven't visited yet
        unvisitedStates =
          [ (cost, state)
            | (state, cost) <- M.toList costs,
              not $ state `S.member` visited
          ]

        -- If there are unvisited states, get the one with minimum cost
        currentState =
          if null unvisitedStates
            then error "No path found"
            else snd $ minimum unvisitedStates

        currentCost = M.findWithDefault maxBound currentState costs

        -- Mark current state as visited
        newVisited = S.insert currentState visited

        -- Get all possible moves from current state
        possibleMoves = getNextMoves map' currentState

        -- Update costs for neighboring states
        newCosts = foldr updateCost costs possibleMoves

        updateCost (nextState, moveCost) costMap =
          let newCost = currentCost + moveCost
              oldCost = M.findWithDefault maxBound nextState costMap
           in if newCost < oldCost
                then M.insert nextState newCost costMap
                else costMap

        -- Get all costs for reaching the end position
        endStateCosts =
          [ cost
            | ((pos, _), cost) <- M.toList costs,
              pos == end
          ]

part1 :: IO ()
part1 = do
  input <- TIO.readFile "input.txt"
  let (map', start, end) = parseMap input
  print $ findShortestPath map' start end

main :: IO ()
main = part1
