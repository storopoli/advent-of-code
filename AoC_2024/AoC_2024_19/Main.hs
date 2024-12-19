{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.Array.ST
import Data.Array.Unboxed
import Data.List (sort)
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

main :: IO ()
main = part1
