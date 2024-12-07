module Main where

import Control.Monad (replicateM)
import Data.List (permutations)
import Data.Text (pack, splitOn, unpack)
import Data.Text qualified as T

-- Part 1

type Output = Int

type Operand = Int

type Equation = (Output, [Operand])

data Operator = Add | Multiply deriving (Show, Eq)

-- I HATE THIS TEXT LIBRARY. Why can't you be friends with String?
parseEquation :: String -> Equation
parseEquation = (\xs -> (read (head xs), map read (words (last xs)))) . map unpack . splitOn (pack ":") . pack

applyOperators :: [Operator] -> [Operand] -> Operand
applyOperators ops (x : xs) = foldl (\acc (op, y) -> applyOperator op acc y) x (zip ops xs)
applyOperators _ _ = 0

applyOperator :: Operator -> Operand -> Operand -> Operand
applyOperator Add = (+)
applyOperator Multiply = (*)

allOperatorCombinations :: Int -> [[Operator]]
allOperatorCombinations n = replicateM n [Add, Multiply]

-- Boom! All possible combinations of `[Operand]` and `[Operator]` in a `Equation` that matches the `Output`
isValidEquation :: Equation -> Bool
isValidEquation (output, operands) =
  any
    (\ops -> applyOperators ops operands == output)
    (allOperatorCombinations (length operands - 1))

part1 :: IO ()
part1 = do
  input <- map parseEquation . lines <$> readFile "input.txt"
  let validEquations = filter isValidEquation input
  let validResults = map fst validEquations
  print $ sum validResults

main :: IO ()
main = part1
