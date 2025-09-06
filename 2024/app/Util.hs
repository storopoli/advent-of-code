module Util where

-- Common utilities for Advent of Code solutions
-- Add utility functions here as needed

{- | Helper to create day functions that take input as parameter
Usage: withInput $ \input -> ... process input ...
-}
withInput :: (String -> IO ()) -> String -> IO ()
withInput f = f
