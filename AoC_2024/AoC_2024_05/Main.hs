module Main where

import qualified Data.IntMap as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import Data.Text (lines, tails, unpack, Text)
import Data.List (sort, elemIndex)
import Data.Maybe (fromMaybe)


-- Part 1

-- |Map for where each page is the key and the value is a list of the pages it should precede
type PrecedenceMap = Map.IntMap [Int]

-- |A list of pages that represents an update
type Update = [Int]

-- |Splits input into a two paragraph pair
--
-- Finds the first line that is empty and splits the list at that point
splitInput :: [Text] -> ([Text], [Text])
splitInput = break T.null

-- |Parses a list of precedences into a precedence map
--
-- List of precedences is a list of pairs of the form "a|b" where a is an integer and b is a list of integers.
-- `a` is the key and `b` are a list of pages that `a` should precede.
parsePrecedences :: [(Int, Int)] -> PrecedenceMap
parsePrecedences ts = Map.fromListWith (++) [(a, [b]) | (a, b) <- ts]

-- |Parses a single "a|b" string into a tuple (a, b)
parsePair :: Text -> (Int, Int)
parsePair txt = case T.splitOn (T.pack "|") txt of
    [a, b] -> (read (unpack a), read (unpack b))
    _      -> error $ "Invalid format: " ++ T.unpack txt

-- |Parses a list of `Text` into a list of `Update`
parseUpdates :: [Text] -> [Update]
parseUpdates = map (map parseInt . T.splitOn (T.pack ","))

-- |Parses a single `Text` into an `Int`
parseInt :: Text -> Int
parseInt txt = case TR.decimal txt of
    Right (n, _) -> n
    Left err     -> error $ "Could not parse integer: " ++ err

-- |Extracts all pairs from a list
pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

-- |Validates an update against a precedence map
--
-- An update is valid if the ordering of the pages respects the precedence map
validateUpdate :: Update -> PrecedenceMap -> Bool
validateUpdate update precedenceMap =
    all isValidRule relevantRules
  where
    relevantRules = [(a, b) |
        (a, bs) <- Map.toList precedenceMap,
        a `elem` update,
        b <- bs,
        b `elem` update]
    isValidRule (a, b) = index a < index b
    index x = fromMaybe maxBound (elemIndex x update)

-- |Finds the middle page of an update
middlePage :: Update -> Int
middlePage update = update !! (length update `div` 2)

part1 :: IO ()
part1 = do
    input <-  T.lines <$> TIO.readFile "input.txt"
    let (before, after) = filter (not . T.null) <$> splitInput input
    let precedenceMap = parsePrecedences (map parsePair before)
    let updates = parseUpdates after
    let validUpdates = filter (`validateUpdate` precedenceMap) updates
    let middlePages = map middlePage validUpdates
    print $ sum middlePages

main :: IO ()
main = part1
