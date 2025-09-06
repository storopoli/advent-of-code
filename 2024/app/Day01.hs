module Day01 (part1, part2) where

import Util (withInput)

-- Part 1

{- | Quicksort implementation.

 Taken from https://wiki.haskell.org/Introduction#Quicksort_in_Haskell
-}
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = [] -- base case: empty list
quicksort (p : xs) = quicksort lesser ++ [p] ++ quicksort greater
  where
    lesser = filter (< p) xs -- filter elements less than the pivot
    greater = filter (>= p) xs -- filter elements greater than or equal to the pivot

part1 :: String -> IO ()
part1 = withInput $ \file ->
    let pairs = map words $ lines file -- split on spaces
    -- sort the numbers
        leftNums = quicksort $ map (read . head) pairs
        rightNums = quicksort $ map (read . last) pairs
     in -- calculate the absolute difference
        print $ sum $ map abs $ zipWith (-) leftNums rightNums

-- Part 2

-- | Counts the number of times a value appears in a list.
count :: (Eq a) => a -> [a] -> Int
count x = length . filter (x ==)

{- | Calculates the number of occurrences of each number in the right list
 and multiplies it with the corresponding number in the left list.
-}
getNumberScore :: [Int] -> Int -> Int
getNumberScore rightNums num = num * count num rightNums

{- | Calculates the similarity score between two lists of integers.

 For each number in the left list, multiply it by how many times it appears
 in the right list, then sum all these products.
-}
similarityScore :: [Int] -> [Int] -> Int
similarityScore leftNums rightNums = sum $ map (getNumberScore rightNums) leftNums

part2 :: String -> IO ()
part2 = withInput $ \file ->
    let pairs = map words $ lines file -- split on spaces
    -- sort the numbers
        leftNums = quicksort $ map (read . head) pairs
        rightNums = quicksort $ map (read . last) pairs
     in -- calculate the similarity score
        print $ similarityScore leftNums rightNums
