module Main where

-- Using Text.Regex.TDFA for regex matching.
-- According to the docs (https://hackage.haskell.org/package/regex-tdfa-1.3.2.2/docs/Text-Regex-TDFA.html)
-- it is more correct than the system regex library.
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

-- Part 1

-- |Splits a string into substrings that have `mul(x,y)`,
-- where x and y are numbers,
-- returning a list of all such substrings.
splitMul :: String -> [String]
splitMul str = getAllTextMatches (str =~ mulRegex)
    where
        -- mulRegex = "mul\\((\\d+),(\\d+)\\)"
        -- fuck regex this is the right one
        mulRegex = "mul\\([0-9]+,[0-9]+\\)"

-- |Extracts the numbers from a `mul(x,y)` substring.
parseMul :: String -> (Int, Int)
parseMul str = (read x, read y)
    where
        mulRegex = "mul\\(([0-9]+),([0-9]+)\\)"
        result = str =~ mulRegex :: (String, String, String, [String])
        submatches = case result of
            (_, _, _, submatches) -> submatches
        [x, y] = submatches

-- |Scans the line for occurences of `mul(x,y)` and returns a list of pairs.
--
-- Finds an occurrence of `mul` then checks the next two words are numbers,
-- returning a list of all such pairs.
parseLine :: String -> [(Int, Int)]
parseLine str = map parseMul $ splitMul str


-- | Acummulates the product of all the pairs of numbers in the list.
accumulate :: [(Int, Int)] -> Int
accumulate = sum . map (uncurry (*))

part1 :: IO ()
part1 = readFile "input.txt" >>= \input ->
    print $ sum . map (accumulate . parseLine) . lines $ input

main :: IO()
main = part1
