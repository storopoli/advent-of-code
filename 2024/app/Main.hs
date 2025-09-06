{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Day09
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18
import Day19
import Day20
import Day21
import Day22
import Day23
import Day24
import Day25
import Options.Applicative

data Cli = Cli
    { day :: Integer
    , input_file :: FilePath
    , part :: Integer
    }
    deriving (Show)

cli :: Parser Cli
cli =
    Cli
        <$> option
            auto
            (long "day" <> short 'd')
        <*> strOption (long "input-file" <> short 'i')
        <*> option auto (long "part" <> short 'p')

main :: IO ()
main = do
    parsedCli <- execParser $ info (cli <**> helper) fullDesc
    print parsedCli

    input <- readFile parsedCli.input_file

    case (parsedCli.day, parsedCli.part) of
        (1, 1) -> Day01.part1 input
        (1, 2) -> Day01.part2 input
        (2, 1) -> Day02.part1 input
        (2, 2) -> Day02.part2 input
        (3, 1) -> Day03.part1 input
        (3, 2) -> Day03.part2 input
        (4, 1) -> Day04.part1 input
        (4, 2) -> Day04.part2 input
        (5, 1) -> Day05.part1 input
        (5, 2) -> Day05.part2 input
        (6, 1) -> Day06.part1 input
        (6, 2) -> Day06.part2 input
        (7, 1) -> Day07.part1 input
        (7, 2) -> Day07.part2 input
        (8, 1) -> Day08.part1 input
        (8, 2) -> Day08.part2 input
        (9, 1) -> Day09.part1 input
        (9, 2) -> error "Day 09 part 2 not implemented"
        (10, 1) -> Day10.part1 input
        (10, 2) -> Day10.part2 input
        (11, 1) -> Day11.part1 input
        (11, 2) -> error "Day 11 part 2 not implemented"
        (12, 1) -> Day12.part1 input
        (12, 2) -> error "Day 12 part 2 not implemented"
        (13, 1) -> Day13.part1 input
        (13, 2) -> error "Day 13 part 2 not implemented"
        (14, 1) -> Day14.part1 input
        (14, 2) -> error "Day 14 part 2 not implemented"
        (15, 1) -> Day15.part1 input
        (15, 2) -> error "Day 15 part 2 not implemented"
        (16, 1) -> Day16.part1 input
        (16, 2) -> error "Day 16 part 2 not implemented"
        (17, 1) -> Day17.part1 input
        (17, 2) -> error "Day 17 part 2 not implemented"
        (18, 1) -> Day18.part1 input
        (18, 2) -> Day18.part2 input
        (19, 1) -> Day19.part1 input
        (19, 2) -> Day19.part2 input
        (20, 1) -> Day20.part1 input
        (20, 2) -> Day20.part2 input
        (21, 1) -> Day21.part1 input
        (21, 2) -> Day21.part2 input
        (22, 1) -> Day22.part1 input
        (22, 2) -> Day22.part2 input
        (23, 1) -> Day23.part1 input
        (23, 2) -> Day23.part2 input
        (24, 1) -> Day24.part1 input
        (24, 2) -> Day24.part2 input
        (25, 1) -> Day25.part1 input
        (25, 2) -> Day25.part2 input
        _ -> error "Day not implemented"

    return ()
