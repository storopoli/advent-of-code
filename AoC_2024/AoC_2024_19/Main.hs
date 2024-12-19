{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.OldList (sortBy)

parseInput :: Text -> ([Text], [Text])
parseInput input =
  let [patterns, designs] = T.splitOn "\n\n" input
      patternList = map T.strip $ T.splitOn "," patterns
      designList = T.lines designs
   in (patternList, designList)

-- Check if a design can be made from patterns
isPossible :: [Text] -> Text -> Bool
isPossible patterns design =
  let n = T.length design
      dp = runSTUArray $ do
        arr <- newArray (0, n) False
        writeArray arr 0 True -- Empty string is always possible
        forM_ [0 .. n - 1] $ \i -> do
          canReachI <- readArray arr i
          when canReachI $
            forM_ patterns $ \pat -> do
              let patLen = T.length pat
                  endPos = i + patLen
              when (endPos <= n && T.isPrefixOf pat (T.drop i design)) $
                writeArray arr endPos True
        return arr
   in dp ! n

part1 :: IO ()
part1 = do
  input <- TIO.readFile "input.txt"
  let (patterns, designs) = parseInput input
  -- Sort patterns by length (descending) for better performance
  let sortedPatterns = sortByLength patterns
  let possible = length $ filter (isPossible sortedPatterns) designs
  putStrLn $ "Number of possible designs: " ++ show possible
  where
    sortByLength = sortBy (\a b -> compare (T.length b) (T.length a))

-- Part 2

-- Pre-compute pattern matches for each position
type MatchCache = Array Int [Text]

buildMatchCache :: [Text] -> Text -> MatchCache
buildMatchCache patterns text =
  array
    (0, T.length text)
    [ ( i,
        [ p | p <- patterns, T.length p <= T.length rest, T.isPrefixOf p rest
        ]
      )
      | i <- [0 .. T.length text],
        let rest = T.drop i text
    ]

countWays2 :: [Text] -> Text -> Integer
countWays2 patterns design =
  let cache = buildMatchCache patterns design
      memo = runST $ do
        arr <- newArray (0, T.length design) 0 :: ST s (STArray s Int Integer)
        -- Base case: empty string can be made in 1 way
        writeArray arr (T.length design) 1
        -- Fill array from right to left
        forM_ [T.length design - 1, T.length design - 2 .. 0] $ \pos -> do
          let matches = cache ! pos
          total <- sum <$> mapM (\p -> readArray arr (pos + T.length p)) matches
          writeArray arr pos total
        arr' <- freeze arr
        return (arr' :: Array Int Integer)
   in memo ! 0

part2 :: IO ()
part2 = do
  input <- TIO.readFile "input.txt"
  let (patterns, designs) = parseInput input
  let sortedPatterns = sortByLength patterns

  let totalWays =
        sum $
          map (countWays2 sortedPatterns) $
            filter (isPossible sortedPatterns) designs
  putStrLn $ "Part 2 - Total number of ways: " ++ show totalWays
  where
    sortByLength = sortBy (\a b -> compare (T.length b) (T.length a))

main :: IO ()
-- main = part1
main = part2
