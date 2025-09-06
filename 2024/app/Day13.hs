module Day13 (part1) where

import Data.Char (isDigit)
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Util (withInput)

-- | Represents a claw machine with button movements and prize location
data Machine = Machine
    { buttonA :: (Int, Int)
    -- ^ Movement for button A (X,Y)
    , buttonB :: (Int, Int)
    -- ^ Movement for button B (X,Y)
    , prize :: (Int, Int)
    -- ^ Prize location (X,Y)
    }
    deriving (Show)

-- | Extract signed number after X or Y
extractNumber :: T.Text -> T.Text -> Maybe Int
extractNumber marker t =
    case T.breakOn marker t of
        (_, rest) | T.null rest -> Nothing
        (_, rest) ->
            let numText = T.takeWhile isDigit $ T.dropWhile (not . isDigit) rest
             in if T.null numText
                    then Nothing
                    else Just $ read $ T.unpack numText

-- | Parse coordinates from a text line
parseCoords :: T.Text -> Maybe (Int, Int)
parseCoords line = do
    x <- extractNumber "X" line
    y <- extractNumber "Y" line
    return (x, y)

-- | Parse a single machine from three lines of text
parseMachine :: [T.Text] -> Maybe Machine
parseMachine [aLine, bLine, pLine] = do
    a <- parseCoords aLine
    b <- parseCoords bLine
    p <- parseCoords pLine
    Just $ Machine a b p
parseMachine _ = Nothing

-- | Parse all machines from input text
parseMachines :: T.Text -> [Machine]
parseMachines =
    mapMaybe parseMachine
        . chunksOf 3
        . filter (not . T.null)
        . T.lines
  where
    chunksOf :: Int -> [a] -> [[a]]
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Try all possible combinations of button presses
findSolution :: Machine -> Maybe (Int, Int)
findSolution (Machine (ax, ay) (bx, by) (px, py)) =
    case filter isValid [(m, n) | m <- [0 .. 100], n <- [0 .. 100]] of
        [] -> Nothing
        ((m, n) : _) -> Just (m, n)
  where
    isValid (m, n) =
        m * ax + n * bx == px
            && m * ay + n * by == py

-- | Calculate tokens needed to win a prize on a machine
solveForMachine :: Machine -> Maybe Int
solveForMachine machine =
    case findSolution machine of
        Just (m, n) -> Just (3 * m + n) -- 3 tokens for A, 1 token for B
        Nothing -> Nothing

-- | Solve the complete puzzle
solve :: T.Text -> Int
solve input =
    let machines = parseMachines input
        solutions = mapMaybe solveForMachine machines
     in sum solutions

part1 :: String -> IO ()
part1 = withInput $ \inputStr -> do
    let input = T.pack inputStr
    print $ "Total tokens needed: " ++ show (solve input)
