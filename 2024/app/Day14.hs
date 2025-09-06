{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Day14 (part1) where

import Control.Monad.State
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Util (withInput)

{- | Position of the Robot in x and y directions.

- Positive x is right, negative x is left.
- Positive y is down, negative y is up.
-}
data Position = Position
    { x :: Int
    , y :: Int
    }
    deriving (Show)

-- | Velocity of the Robot in both x and y directions in coordinates per second.
type Velocity = Position

-- | Robot
type Robot = (Position, Velocity)

{- | Grid of Robots tiles.

The first coordinate is the width (X-axis)
and the second coordinate is the height (Y-axis).

- 11x 7 for `test_input.txt`
- 101x103 for `input.txt`
-}
type Grid = (Int, Int)

-- | GridState Monad for handling grid operations
newtype GridState a = GridState {runGridState :: State Grid a}
    deriving (Functor, Applicative, Monad, MonadState Grid)

-- | Execute a GridState computation with an initial grid
runGrid :: GridState a -> Grid -> a
runGrid gs = evalState (runGridState gs)

-- | Teleport computation in GridState
teleport :: Position -> GridState Position
teleport (Position x y) = do
    (width, height) <- get
    return $ Position (x `mod` width) (y `mod` height)

-- | Simulate single step in GridState
simulateStep :: Robot -> GridState Robot
simulateStep (Position x y, vel@(Position vx vy)) = do
    (width, height) <- get
    let x' = x + vx
        y' = y + vy
    newPos <-
        if x' < 0 || x' >= width || y' < 0 || y' >= height
            then teleport (Position x' y')
            else return (Position x' y')
    return (newPos, vel)

-- | Simulate multiple steps in GridState
simulateSteps :: Int -> [Robot] -> GridState [Robot]
simulateSteps 0 robots = return robots
simulateSteps n robots = do
    newRobots <- mapM simulateStep robots
    simulateSteps (n - 1) newRobots

-- | Count robots in quadrants using GridState
countQuadrants :: [Robot] -> GridState Int
countQuadrants robots = do
    (width, height) <- get
    let midX = width `div` 2
        midY = height `div` 2
        nw = length [(Position px py, v) | (Position px py, v) <- robots, px < midX, py < midY]
        ne = length [(Position px py, v) | (Position px py, v) <- robots, px > midX, py < midY]
        sw = length [(Position px py, v) | (Position px py, v) <- robots, px < midX, py > midY]
        se = length [(Position px py, v) | (Position px py, v) <- robots, px > midX, py > midY]
    return $ nw * ne * sw * se

-- | Parse position remains the same
parsePosition :: Text -> Robot
parsePosition line =
    case T.splitOn " " line of
        [pos, vel] ->
            case (T.splitOn "=" pos, T.splitOn "=" vel) of
                ([_, posCoords], [_, velCoords]) ->
                    case (T.splitOn "," posCoords, T.splitOn "," velCoords) of
                        ([x, y], [vx, vy]) ->
                            let x' = read $ T.unpack x
                                y' = read $ T.unpack y  
                                vx' = read $ T.unpack vx
                                vy' = read $ T.unpack vy
                             in (Position x' y', Position vx' vy')
                        _ -> (Position 0 0, Position 1 1) -- Fallback for invalid format
                _ -> (Position 0 0, Position 1 1) -- Fallback for invalid format
        _ -> (Position 0 0, Position 1 1) -- Fallback for invalid format

-- | Main computation in GridState
computeSafetyFactor :: [Robot] -> Int -> GridState Int
computeSafetyFactor robots steps = do
    finalRobots <- simulateSteps steps robots
    countQuadrants finalRobots

part1 :: String -> IO ()
part1 = withInput $ \inputStr -> do
    let input = T.lines (T.pack inputStr)
    let robots = map parsePosition input
        grid = (101, 103)
        result = runGrid (computeSafetyFactor robots 100) grid
    print result
