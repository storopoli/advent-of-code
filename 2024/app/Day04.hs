{-# LANGUAGE LambdaCase #-}

module Day04 (part1, part2) where

import Util (withInput)

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

-- | Parses a line of input into a list of integers.
parseLine :: String -> [Int]
parseLine =
    map
        ( \case
            'X' -> 1
            'M' -> 2
            'A' -> 3
            'S' -> 4
            _ -> error "Invalid character"
        )

-- | Parses the entire input into a list of lines.
parseInput :: String -> [[Int]]
parseInput = map parseLine . lines

-- | Gets all sequences of length `n` in a specific direction.
getSequences :: Int -> [[Int]] -> (Int, Int) -> [[Int]]
getSequences n mat (dx, dy) =
    [ take n $ map (getElem mat) $ take n $ iterate (addPos (dx, dy)) (i, j)
    | i <- [0 .. length mat - 1]
    , j <- [0 .. length (head mat) - 1]
    , validSequence mat n (i, j) (dx, dy)
    ]

-- | Checks if a sequence of length `n` in a specific direction is valid.
validSequence :: [[a]] -> Int -> (Int, Int) -> (Int, Int) -> Bool
validSequence mat n (i, j) (dx, dy) = all (uncurry (validPosition mat)) positions
  where
    positions = take n $ iterate (addPos (dx, dy)) (i, j)

-- | Checks if a position is valid in a matrix.
validPosition :: [[a]] -> Int -> Int -> Bool
validPosition mat i j = i >= 0 && i < length mat && j >= 0 && j < length (head mat)

-- | Gets an element from a matrix.
getElem :: [[a]] -> (Int, Int) -> a
getElem mat (i, j) = mat !! i !! j

-- | Adds two positions.
addPos :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPos (dx, dy) (x, y) = (x + dx, y + dy)

-- | All possible directions.
directions :: [(Int, Int)]
directions =
    [ (0, 1) -- Horizontal Left to Right
    , (0, -1) -- Horizontal Right to Left
    , (1, 0) -- Vertical Top to Bottom
    , (-1, 0) -- Vertical Bottom to Top
    , (1, 1) -- Diagonal Top-Left to Bottom-Right
    , (-1, -1) -- Diagonal Bottom-Right to Top-Left
    , (1, -1) -- Diagonal Top-Right to Bottom-Left
    , (-1, 1) -- Diagonal Bottom-Left to Top-Right
    ]

-- | Extracts all sequences of length `n` in all directions.
extractAllSequences :: Int -> [[Int]] -> [[Int]]
extractAllSequences n mat = concatMap (getSequences n mat) directions

-- Part 1
part1 :: String -> IO ()
part1 = withInput $ \inputStr -> do
    let input = parseInput inputStr
    let pred xs = xs == [1, 2, 3, 4]
    let allSeqs = extractAllSequences 4 input
    let result = length $ filter pred allSeqs
    print result

-- Part 2

-- | Parses the input grid into a list of lists of characters.
parseInput2 :: String -> [[Char]]
parseInput2 = lines

-- | Checks if the positions are within the bounds of the grid.
inBounds :: [[a]] -> (Int, Int) -> Bool
inBounds grid (i, j) =
    let h = length grid
        w = length (head grid)
     in i >= 0 && i < h && j >= 0 && j < w

-- | Retrieves the letters at specified positions.
getLetters :: [[Char]] -> [(Int, Int)] -> Maybe String
getLetters grid positions =
    if all (inBounds grid) positions
        then Just [grid !! i !! j | (i, j) <- positions]
        else Nothing

-- | Checks if a sequence matches "MAS" or "SAM".
matchesMAS :: String -> Bool
matchesMAS seq = seq == "MAS" || seq == "SAM"

-- | Checks if an X-MAS is formed at position (i, j).
isXMAS :: [[Char]] -> Int -> Int -> Bool
isXMAS grid i j =
    let
        -- Diagonal positions
        diag1 = [(i - 1, j - 1), (i, j), (i + 1, j + 1)] -- Top-left to Bottom-right
        diag2 = [(i - 1, j + 1), (i, j), (i + 1, j - 1)] -- Top-right to Bottom-left
     in
        case (getLetters grid diag1, getLetters grid diag2) of
            (Just d1, Just d2) -> matchesMAS d1 && matchesMAS d2
            _ -> False

-- | Counts the number of X-MAS instances in the grid.
countXMAS :: [[Char]] -> Int
countXMAS grid =
    let h = length grid
        w = length (head grid)
        positions = [(i, j) | i <- [1 .. h - 2], j <- [1 .. w - 2]]
     in length $ filter (uncurry (isXMAS grid)) positions

-- | Main function to read the input and compute the result.
part2 :: String -> IO ()
part2 = withInput $ \inputStr -> do
    let input = parseInput2 inputStr
    let result = countXMAS input
    print result
