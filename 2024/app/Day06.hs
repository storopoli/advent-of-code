{-# LANGUAGE FlexibleContexts #-}

module Day06 (part1, part2) where

-- Part 1

import Control.Monad.ST
import Data.Array.ST
import Data.List (nub)
import Util (withInput)

data Dir = U | R | D | L deriving (Show, Eq)

turnRight :: Dir -> Dir
turnRight U = R
turnRight R = D
turnRight D = L
turnRight L = U

delta :: Dir -> (Int, Int)
delta U = (-1, 0)
delta R = (0, 1)
delta D = (1, 0)
delta L = (0, -1)

parseInput :: [String] -> ([String], (Int, Int, Dir))
parseInput grid =
    case findGuard grid of
        Just s -> (grid, s)
        Nothing -> error "No guard found!"

findGuard :: [String] -> Maybe (Int, Int, Dir)
findGuard grid = go 0
  where
    h = length grid
    w = if null grid then 0 else length (head grid)
    go r
        | r >= h = Nothing
        | otherwise = case findInRow r 0 of
            Just d -> Just d
            Nothing -> go (r + 1)
    findInRow r c
        | c >= w = Nothing
        | otherwise =
            let ch = (grid !! r) !! c
             in case charDir ch of
                    Just dir -> Just (r, c, dir)
                    Nothing -> findInRow r (c + 1)

charDir :: Char -> Maybe Dir
charDir '^' = Just U
charDir '>' = Just R
charDir 'v' = Just D
charDir '<' = Just L
charDir _ = Nothing

outOfBounds :: [String] -> (Int, Int) -> Bool
outOfBounds g (r, c) = r < 0 || c < 0 || r >= length g || c >= length (head g)

isObstacle :: [String] -> (Int, Int) -> Bool
isObstacle g (r, c)
    | outOfBounds g (r, c) = False
    | otherwise = (g !! r) !! c == '#'

simulate :: [String] -> (Int, Int, Dir) -> [(Int, Int)]
simulate grid (sr, sc, sd) = reverse (go (sr, sc) sd [(sr, sc)])
  where
    go pos dir visited =
        let (dr, dc) = delta dir
            frontPos = (fst pos + dr, snd pos + dc)
         in if isObstacle grid frontPos
                then
                    -- Turn right
                    go pos (turnRight dir) visited
                else
                    -- Move forward
                    let newPos = frontPos
                     in if outOfBounds grid newPos
                            then visited
                            else go newPos dir (newPos : visited)

part1 :: String -> IO ()
part1 = withInput $ \inputStr -> do
    let input = lines inputStr
    let (grid, startState) = parseInput input
    let visited = simulate grid startState
    print (length (nub visited))

-- Part 2

-- Convert direction to an Int for indexing
dirToInt :: Dir -> Int
dirToInt U = 0
dirToInt R = 1
dirToInt D = 2
dirToInt L = 3

gridChar :: [String] -> (Int, Int) -> Char
gridChar g (r, c) = (g !! r) !! c

possibleObstructionPositions :: [String] -> (Int, Int) -> [(Int, Int)]
possibleObstructionPositions grid (sr, sc) =
    [ (r, c)
    | r <- [0 .. length grid - 1]
    , c <- [0 .. length (head grid) - 1]
    , not (r == sr && c == sc)
    , gridChar grid (r, c) /= '#'
    ]

placeObstacle :: [String] -> (Int, Int) -> [String]
placeObstacle grid (r, c) =
    let row = grid !! r
        newRow = take c row ++ "#" ++ drop (c + 1) row
     in take r grid ++ [newRow] ++ drop (r + 1) grid

simulateWithLoop :: [String] -> (Int, Int) -> Dir -> Bool
simulateWithLoop grid startPos startDir = runST $ do
    let h = length grid
        w = length (head grid)
        -- Each state is (r, c, dir)
        -- Encode this as: index = dirToInt(dir) * (h*w) + r*w + c
        idx (r, c, dir) = dirToInt dir * (h * w) + r * w + c

    visitedStates <- newArray (0, (4 * h * w) - 1) False :: ST s (STArray s Int Bool)

    let step (r, c) dir = do
            if outOfBounds grid (r, c)
                then return False
                else do
                    let i = idx (r, c, dir)
                    visited <- readArray visitedStates i
                    if visited
                        then return True -- loop detected
                        else do
                            writeArray visitedStates i True
                            let (dr, dc) = delta dir
                                frontPos = (r + dr, c + dc)
                            if isObstacle grid frontPos
                                then step (r, c) (turnRight dir) -- turn right in place
                                else
                                    if outOfBounds grid frontPos
                                        then return False -- guard leaves map
                                        else step frontPos dir

    step startPos startDir

causesLoop :: [String] -> (Int, Int, Dir) -> (Int, Int) -> Bool
causesLoop grid (sr, sc, sd) pos =
    simulateWithLoop (placeObstacle grid pos) (sr, sc) sd

part2 :: String -> IO ()
part2 = withInput $ \inputStr -> do
    let input = lines inputStr
    let (grid, (sr, sc, sd)) = parseInput input
        candidates = possibleObstructionPositions grid (sr, sc)
        loopPositions = [pos | pos <- candidates, causesLoop grid (sr, sc, sd) pos]
    print (length loopPositions)
