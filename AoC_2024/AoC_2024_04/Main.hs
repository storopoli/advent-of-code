{-# LANGUAGE LambdaCase #-}
module Main where

-- Part 1
-- I'm gonna solve this by creating a 2D array of the characters
-- where X=1, M=2, A=3, S=4,
-- then checking if they appear in sequence in:
--
-- - Row
-- - Backward Row
-- - Column
-- - Backward Column
-- - Diagonal
-- - Backward Diagonal
--
-- I think I can get away with just summing up 4 consecutive numbers
-- and checking if the sum is 10.

-- Parsing stuff

-- |Parses a line of input into a list of integers.
parseLine :: String -> [Int]
parseLine = map (\case
    'X' -> 1
    'M' -> 2
    'A' -> 3
    'S' -> 4
    _   -> error "Invalid character"
    )

-- |Parses the entire input into a list of lines.
parseInput :: String -> [[Int]]
parseInput = map parseLine . lines

-- |Gets all sequences of length `n` in a specific direction.
getSequences :: Int -> [[Int]] -> (Int, Int) -> [[Int]]
getSequences n mat (dx, dy) =
    [ take n $ map (getElem mat) $ take n $ iterate (addPos (dx, dy)) (i, j)
    | i <- [0 .. length mat - 1]
    , j <- [0 .. length (head mat) - 1]
    , validSequence mat n (i, j) (dx, dy)
    ]

-- |Checks if a sequence of length `n` in a specific direction is valid.
validSequence :: [[a]] -> Int -> (Int, Int) -> (Int, Int) -> Bool
validSequence mat n (i, j) (dx, dy) = all (uncurry (validPosition mat)) positions
  where positions = take n $ iterate (addPos (dx, dy)) (i, j)

-- |Checks if a position is valid in a matrix.
validPosition :: [[a]] -> Int -> Int -> Bool
validPosition mat i j = i >= 0 && i < length mat && j >= 0 && j < length (head mat)

-- |Gets an element from a matrix.
getElem :: [[a]] -> (Int, Int) -> a
getElem mat (i, j) = mat !! i !! j

-- |Adds two positions.
addPos :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPos (dx, dy) (x, y) = (x + dx, y + dy)

-- |All possible directions.
directions :: [(Int, Int)]
directions =
    [ (0, 1)   -- Horizontal Left to Right
    , (0, -1)  -- Horizontal Right to Left
    , (1, 0)   -- Vertical Top to Bottom
    , (-1, 0)  -- Vertical Bottom to Top
    , (1, 1)   -- Diagonal Top-Left to Bottom-Right
    , (-1, -1) -- Diagonal Bottom-Right to Top-Left
    , (1, -1)  -- Diagonal Top-Right to Bottom-Left
    , (-1, 1)  -- Diagonal Bottom-Left to Top-Right
    ]

-- |Extracts all sequences of length `n` in all directions.
extractAllSequences :: Int -> [[Int]] -> [[Int]]
extractAllSequences n mat = concatMap (getSequences n mat) directions

-- Part 1
part1 :: IO ()
part1 = do
    input <- parseInput <$> readFile "input.txt"
    let pred xs = xs == [1,2,3,4]
    let allSeqs = extractAllSequences 4 input
    let result = length $ filter pred allSeqs
    print result


main :: IO ()
main = part1
