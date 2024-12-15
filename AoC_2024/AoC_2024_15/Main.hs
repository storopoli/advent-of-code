{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

-- | Core Types
type Coordinate = (Int, Int)

type Robot = Coordinate

type WarehouseMap = M.Map Coordinate Tile

-- | Direction for robot movement
data Direction = Up | Down | Left | Right deriving (Show, Eq)

-- | Tile types in the warehouse
data Tile = Wall | Floor | Robot | Box deriving (Show, Eq)

-- | State of the warehouse including map and robot position
data WarehouseState = WarehouseState
  { warehouseMap :: WarehouseMap,
    robotPos :: Robot
  }
  deriving (Show)

-- | Parse a character into a tile
parseTile :: Char -> Tile
parseTile = \case
  '#' -> Wall
  '.' -> Floor
  '@' -> Robot
  'O' -> Box
  c -> error $ "Invalid tile: " <> show c

-- | Parse the warehouse map from text input
parseMap :: Text -> [(Coordinate, Tile)]
parseMap input = do
  (y, line) <- zip [0 ..] (T.lines input)
  (x, char) <- zip [0 ..] (T.unpack line)
  return ((x, y), parseTile char)

-- | Parse a single movement direction
parseMovement :: Text -> Direction
parseMovement = \case
  "^" -> Up
  "v" -> Down
  "<" -> Main.Left
  ">" -> Main.Right
  c -> error $ "Invalid movement: " <> T.unpack c

-- | Parse sequence of movements
parseMovements :: Text -> [Direction]
parseMovements = map parseMovement . T.chunksOf 1 . T.filter (\c -> c /= '\r' && c /= '\n')

-- | Initialize warehouse state from tile list
initialize :: [(Coordinate, Tile)] -> WarehouseState
initialize tiles =
  let gMap = M.fromList tiles
      rPos = head [pos | (pos, tile) <- tiles, tile == Robot]
   in WarehouseState gMap rPos

-- | Calculate new position based on direction
getNewPosition :: Direction -> Coordinate -> Coordinate
getNewPosition dir (x, y) = case dir of
  Up -> (x, y - 1)
  Down -> (x, y + 1)
  Main.Left -> (x - 1, y)
  Main.Right -> (x + 1, y)

-- | Check if boxes can be pushed in a direction
canPushBoxes :: WarehouseMap -> Direction -> Coordinate -> Maybe [Coordinate]
canPushBoxes map' dir start = go start []
  where
    go pos boxes = case M.lookup pos map' of
      Just Box ->
        let nextPos = getNewPosition dir pos
         in case M.lookup nextPos map' of
              Just Floor -> Just (pos : boxes)
              Just Box -> go nextPos (pos : boxes)
              _ -> Nothing
      Just Floor -> Just boxes
      _ -> Nothing

-- | Move robot in specified direction
moveRobot :: Direction -> State WarehouseState Bool
moveRobot dir = do
  state <- get
  let currentPos = robotPos state
      newPos = getNewPosition dir currentPos
      map' = warehouseMap state

  case M.lookup newPos map' of
    Just Floor -> do
      let newMap = M.insert newPos Robot $ M.insert currentPos Floor map'
      put $ WarehouseState newMap newPos
      return True
    Just Box ->
      case canPushBoxes map' dir newPos of
        Just boxes -> do
          let positions = boxes
              newPositions = map (getNewPosition dir) boxes
              tempMap = foldr (\pos m -> M.insert pos Floor m) map' positions
              finalMap = foldr (\pos m -> M.insert pos Box m) tempMap newPositions
              robotMap = M.insert newPos Robot $ M.insert currentPos Floor finalMap
          put $ WarehouseState robotMap newPos
          return True
        Nothing -> return False
    _ -> return False

-- | Execute sequence of movements
executeMovements :: [Direction] -> State WarehouseState [Bool]
executeMovements = mapM moveRobot

-- | Calculate GPS coordinates sum for all boxes
calculateGPS :: WarehouseState -> Int
calculateGPS state = sum $ map calcGPS boxPositions
  where
    boxPositions = [pos | (pos, tile) <- M.toList (warehouseMap state), tile == Box]
    calcGPS (x, y) = 100 * y + x

-- | Render warehouse state as string
renderWarehouse :: WarehouseState -> String
renderWarehouse state =
  let map' = warehouseMap state
      coords = M.keys map'
      maxX = maximum $ map fst coords
      maxY = maximum $ map snd coords
      getTile pos = case M.lookup pos map' of
        Just Wall -> '#'
        Just Floor -> '.'
        Just Robot -> '@'
        Just Box -> 'O'
        Nothing -> ' '
   in unlines [[getTile (x, y) | x <- [0 .. maxX]] | y <- [0 .. maxY]]

part1 :: IO ()
part1 = do
  input <- TIO.readFile "input.txt"
  let (mapText, movementsText) = T.breakOn "\n\n" input
  let map' = parseMap mapText
  let movements' = parseMovements $ T.filter (\c -> c /= '\r' && c /= '\n') $ T.drop 2 movementsText
  let initialState = initialize map'
  let (_, finalState) = runState (executeMovements movements') initialState
  print $ calculateGPS finalState

main :: IO ()
main = part1
