module Day07 (part1, part2) where

import Control.Monad (replicateM)
import Data.Text (pack, splitOn, unpack)
import Util (withInput)

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

part1 :: String -> IO ()
part1 = withInput $ \inputStr -> do
    let input = map parseEquation . lines $ inputStr
    let validEquations = filter isValidEquation input
    let validResults = map fst validEquations
    print $ sum validResults

-- Part 2
data Operator2 = Add2 | Multiply2 | Concat deriving (Show, Eq)

applyOperators2 :: [Operator2] -> [Operand] -> Operand
applyOperators2 ops (x : xs) = foldl (\acc (op, y) -> applyOperator2 op acc y) x (zip ops xs)
applyOperators2 _ _ = 0

applyOperator2 :: Operator2 -> Operand -> Operand -> Operand
applyOperator2 Add2 = (+)
applyOperator2 Multiply2 = (*)
applyOperator2 Concat = \x y -> read (show x <> show y)

allOperatorCombinations2 :: Int -> [[Operator2]]
allOperatorCombinations2 n = replicateM n [Add2, Multiply2, Concat]

-- Boom! All possible combinations of `[Operand]` and `[Operator]` in a `Equation` that matches the `Output`
isValidEquation2 :: Equation -> Bool
isValidEquation2 (output, operands) =
    any
        (\ops -> applyOperators2 ops operands == output)
        (allOperatorCombinations2 (length operands - 1))

part2 :: String -> IO ()
part2 = withInput $ \inputStr -> do
    let input = map parseEquation . lines $ inputStr
    let validEquations = filter isValidEquation2 input
    let validResults = map fst validEquations
    print $ sum validResults
