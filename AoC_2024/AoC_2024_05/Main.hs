
module Main where

main :: IO()
main = do
    input <- lines <$> readFile "test_input.txt"
    print input
