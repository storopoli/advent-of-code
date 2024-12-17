{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.State
import Data.Bits (xor)
import Data.List (intercalate)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

-- Data types
data Registers = Registers
  { regA :: Int,
    regB :: Int,
    regC :: Int
  }
  deriving (Show)

data Computer = Computer
  { registers :: Registers,
    instrPointer :: Int,
    program :: [Int],
    output :: [Int]
  }
  deriving (Show)

-- Parse input
parseRegisters :: Text -> Registers
parseRegisters input =
  let values = map (read . T.unpack . last . T.splitOn ": ") $ T.lines input
   in Registers (values !! 0) (values !! 1) (values !! 2)

parseProgram :: Text -> [Int]
parseProgram input =
  map (read . T.unpack) $ T.splitOn "," $ last $ T.splitOn ": " input

-- Helper functions
getComboValue :: Int -> Computer -> Int
getComboValue n comp
  | n <= 3 = n
  | n == 4 = regA $ registers comp
  | n == 5 = regB $ registers comp
  | n == 6 = regC $ registers comp
  | otherwise = error "Invalid combo operand"

-- Instructions
type ComputerState = State Computer

adv :: Int -> ComputerState ()
adv operand = modify $ \comp ->
  let value = regA (registers comp) `div` (2 ^ getComboValue operand comp)
   in comp
        { registers = (registers comp) {regA = value},
          instrPointer = instrPointer comp + 2
        }

bxl :: Int -> ComputerState ()
bxl operand = modify $ \comp ->
  let value = regB (registers comp) `xor` operand
   in comp
        { registers = (registers comp) {regB = value},
          instrPointer = instrPointer comp + 2
        }

bst :: Int -> ComputerState ()
bst operand = modify $ \comp ->
  let value = getComboValue operand comp `mod` 8
   in comp
        { registers = (registers comp) {regB = value},
          instrPointer = instrPointer comp + 2
        }

jnz :: Int -> ComputerState ()
jnz operand = modify $ \comp ->
  if regA (registers comp) /= 0
    then comp {instrPointer = operand}
    else comp {instrPointer = instrPointer comp + 2}

bxc :: Int -> ComputerState ()
bxc _ = modify $ \comp ->
  let value = regB (registers comp) `xor` regC (registers comp)
   in comp
        { registers = (registers comp) {regB = value},
          instrPointer = instrPointer comp + 2
        }

out :: Int -> ComputerState ()
out operand = modify $ \comp ->
  let value = getComboValue operand comp `mod` 8
   in comp
        { output = output comp ++ [value],
          instrPointer = instrPointer comp + 2
        }

bdv :: Int -> ComputerState ()
bdv operand = modify $ \comp ->
  let value = regA (registers comp) `div` (2 ^ getComboValue operand comp)
   in comp
        { registers = (registers comp) {regB = value},
          instrPointer = instrPointer comp + 2
        }

cdv :: Int -> ComputerState ()
cdv operand = modify $ \comp ->
  let value = regA (registers comp) `div` (2 ^ getComboValue operand comp)
   in comp
        { registers = (registers comp) {regC = value},
          instrPointer = instrPointer comp + 2
        }

-- Execute instruction
executeInstruction :: ComputerState ()
executeInstruction = do
  comp <- get
  if instrPointer comp >= length (program comp)
    then return ()
    else do
      let op = program comp !! instrPointer comp
      let operand = program comp !! (instrPointer comp + 1)
      case op of
        0 -> adv operand
        1 -> bxl operand
        2 -> bst operand
        3 -> jnz operand
        4 -> bxc operand
        5 -> out operand
        6 -> bdv operand
        7 -> cdv operand
      executeInstruction

-- Run program
runProgram :: Computer -> [Int]
runProgram comp = output $ execState executeInstruction comp

part1 :: IO ()
part1 = do
  input <- TIO.readFile "input.txt"
  let (registerText, instructionsText) = T.breakOn "\n\n" input
  let registers = parseRegisters registerText
  let program = parseProgram (last $ T.splitOn "\n" instructionsText)
  let initialState = Computer registers 0 program []
  let result = runProgram initialState
  putStrLn $ intercalate "," $ map show result

main :: IO ()
main = part1
