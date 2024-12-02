module Main where

-- |Parses a line of input into a list of integers
parseLine :: String -> [Int]
parseLine = map read . words

-- Part 1

-- |Defines if a sequence is "safe".
--
-- A sequence is safe if either is monotonic increasing or decreasing;
-- and any two adjacent numbers differs by at most 3 and at least 1.
isSafe :: [Int] -> Bool
isSafe xs = 
    -- The trick here is to take a zip of the list with its tail,
    -- this guarantees that we have at least one pair of an element and its successor.
    -- 1st case: monotonic increasing
    (isMonotonic xs && all isSafePair (zip xs (tail xs))) ||
    -- 2nd case: monotonic decreasing
    (isMonotonic (reverse xs) && all isSafePair (zip (reverse xs) (tail (reverse xs))))

-- |Defines if a sequence is monotonic.
isMonotonic :: [Int] -> Bool
isMonotonic [] = True
isMonotonic [_] = True
isMonotonic (x:y:xs) = x <= y && isMonotonic (y:xs)

-- |Defines if two adjacent numbers differ by at most 3 and at least 1.
isSafePair :: (Int, Int) -> Bool
isSafePair (x, y) = abs(x - y) <= 3 && abs(x - y) >= 1

part1 :: IO ()
part1 = readFile "input.txt" >>= \input ->
   print $ length $ filter isSafe $ map parseLine $ lines input

main :: IO ()
main = part1
