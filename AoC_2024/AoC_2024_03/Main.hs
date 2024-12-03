module Main where

-- Using Text.Regex.TDFA for regex matching.
-- According to the docs (https://hackage.haskell.org/package/regex-tdfa-1.3.2.2/docs/Text-Regex-TDFA.html)
-- it is more correct than the system regex library.
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)

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

-- Part 2
-- This is annyoing the easy way is to create a State type that has 
-- an accumulator that can be incremented IF mul is enabled.

-- NOTE: This could be optimized by making the State a Monad and using the
-- Reader Monad to pass the state around.
-- But this is good enough for now.

-- |State for processing multiplications
data State = State {
    accumulator :: Int,
    -- ^ The current value of the accumulator
    mulEnabled :: Bool
    -- ^ Whether multiplications are enabled
} deriving (Show)


-- |Instruction for processing multiplications
data Instruction
    = Do
    -- ^ Enable multiplications
    | Dont
    -- ^ Disable multiplications
    | Mul Int Int
    -- ^ Multiply the two numbers
    deriving (Show)

-- |Parses a single instruction from a string
parseInstruction :: String -> Maybe Instruction
parseInstruction str
    | str == "do()"    = Just Do
    | str == "don't()" = Just Dont
    | "mul(" `isPrefixOf` str = case str =~ "mul\\(([0-9]+),([0-9]+)\\)" :: (String, String, String, [String]) of
        (_, _, _, [x, y]) -> Just $ Mul (read x) (read y)
        _ -> Nothing
    | otherwise = Nothing

-- |Processes a single instruction
processInstruction :: State -> Instruction -> State
processInstruction state Do = state { mulEnabled = True }
processInstruction state Dont = state { mulEnabled = False }
processInstruction state (Mul x y)
    | mulEnabled state = state { accumulator = accumulator state + x * y }
    | otherwise = state


-- |Processes a list of instructions
processInstructions :: [Instruction] -> State -> State
processInstructions instructions state = foldl processInstruction state instructions


-- |Splits a string into substrings that contain instructions
splitInstructions :: String -> [String]
splitInstructions str = getAllTextMatches (str =~ instructionRegex)
    where
        instructionRegex = "(mul\\([0-9]+,[0-9]+\\)|do\\(\\)|don't\\(\\))"

-- |Processes a line of input
processLine :: String -> State -> State
processLine line = processInstructions (mapMaybe parseInstruction (splitInstructions line))

part2 :: IO ()
part2 = readFile "input.txt" >>= \input ->
    print $ accumulator $ processLine input (State 0 True) -- start with 0 and enabled

main :: IO()
-- main = part1
main = part2
