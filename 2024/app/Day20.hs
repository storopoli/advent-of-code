module Day20 (part1, part2) where

import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Util (withInput)

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
            | (y, line) <- zip [0 ..] $ T.lines input
            , (x, c) <- zip [0 ..] $ T.unpack line
            ]
    startList = [(x, y) | ((x, y), c) <- M.toList grid, c == 'S']
    endList = [(x, y) | ((x, y), c) <- M.toList grid, c == 'E']
    start = if null startList then (0, 0) else head startList
    end = if null endList then (1, 1) else head endList

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

part1 :: String -> IO ()
part1 = withInput $ \inputStr -> do
    let input = T.pack inputStr
    let (grid, start, end) = parseInput input
    let distances = findDistances grid start
    case M.lookup end distances of
        Nothing -> putStrLn "Error: Invalid input data - no path from start to end found"
        Just normalTime -> do
            putStrLn $ "Normal path length: " ++ show normalTime
            let cheats = findCheats grid start end normalTime
            let validCheats = filter ((>= 100) . fst) cheats
            putStrLn $
                "Number of cheats saving ≥100 picoseconds: "
                    ++ show (sum [count | (_, count) <- validCheats])

-- Part 2: Find extended cheats
type DistMap = M.Map Pos Int

findCheatPaths2 :: Grid -> Pos -> Pos -> Int -> [(Int, Int)]
findCheatPaths2 grid start end normalTime = do
    let fromStart = findDistances grid start -- Regular distances from start
    let fromEnd = findDistances grid end -- Regular distances from end

    -- Find all pairs of points that could be connected by a cheat sequence
    let cheatPaths = do
            pos1 <- M.keys fromStart
            pos2 <- M.keys fromEnd
            let dist1 = fromStart M.! pos1 -- Distance from start to pos1
            let dist2 = fromEnd M.! pos2 -- Distance from pos2 to end

            -- Check if points can be connected with ≤20 steps
            let manhattanDist = abs (fst pos1 - fst pos2) + abs (snd pos1 - snd pos2)
            guard $ manhattanDist <= 20

            -- Calculate total path length and time saved
            let totalDist = dist1 + manhattanDist + dist2
            let saved = normalTime - totalDist
            guard $ saved >= 2

            return (saved, 1)

    M.toList $ M.fromListWith (+) cheatPaths
  where
    guard True = [()]
    guard False = []

    -- Helper to check if two points can be connected within steps
    canConnect :: Pos -> Pos -> Int -> Bool
    canConnect p1 p2 steps =
        abs (fst p1 - fst p2) + abs (snd p1 - snd p2) <= steps

part2 :: String -> IO ()
part2 = withInput $ \inputStr -> do
    let input = T.pack inputStr
    let (grid, start, end) = parseInput input
    let distances = findDistances grid start
    case M.lookup end distances of
        Nothing -> putStrLn "Error: Invalid input data - no path from start to end found"
        Just normalTime -> do
            putStrLn $ "Normal path length: " ++ show normalTime
            let cheats = findCheatPaths2 grid start end normalTime
            let validCheats = filter ((>= 100) . fst) cheats
            putStrLn $
                "Number of cheats saving ≥100 picoseconds: "
                    ++ show (sum [count | (_, count) <- validCheats])
