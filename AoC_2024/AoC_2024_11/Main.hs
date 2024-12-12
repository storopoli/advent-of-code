module Main where

import Data.Char (digitToInt)
import Data.Text qualified (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

-- Split a number into its left and right halves
splitNum :: Integer -> (Integer, Integer)
splitNum n =
  let s = show n
      len = length s
      mid = len `div` 2
      (left, right) = splitAt mid s
   in (read left, read right)

-- Check if a number has an even number of digits
hasEvenDigits :: Integer -> Bool
hasEvenDigits n = even $ length $ show n

-- Apply transformation rules to a single number
transform :: Integer -> [Integer]
transform 0 = [1]
transform n
  | hasEvenDigits n = let (l, r) = splitNum n in [l, r]
  | otherwise = [n * 2024]

-- Apply transformation to all numbers in the list
step :: [Integer] -> [Integer]
step = concatMap transform

-- Apply transformation n times
iterateN :: Int -> [Integer] -> [Integer]
iterateN 0 xs = xs
iterateN n xs = iterateN (n - 1) (step xs)

-- Part 1
part1 :: IO ()
part1 = do
  input <- T.lines <$> TIO.readFile "input.txt"
  let numbers = concatMap (map (read . T.unpack) . T.words) input
  let result = iterateN 25 numbers
  print $ length result

main :: IO ()
main = part1
