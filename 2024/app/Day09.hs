-- Today I was lazy, my solution was good enough for the test input.
-- But it took ages in the input. So I asked an AI to optimize it.
-- Part 2 is annoying AF.
module Day09 (part1) where

-- stack ghc --package primitive --package vector

import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (runST)
import Data.Char (digitToInt, isDigit)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Vector.Mutable qualified as M
import Util (withInput)

-- Part 1

type FileID = Int

data Segment = FileSeg Int | SpaceSeg Int
    deriving (Show)

parseDiskMap :: T.Text -> [Segment]
parseDiskMap txt =
    let ds = map digitToInt . filter isDigit . T.unpack . T.strip $ txt
     in go ds True
  where
    go [] _ = []
    go (x : xs) isFile
        | isFile = FileSeg x : go xs False
        | otherwise = SpaceSeg x : go xs True

expandLayout :: [Segment] -> [Maybe FileID]
expandLayout segs = concat $ snd $ foldl f (0, []) segs
  where
    f (fileCount, acc) (FileSeg n) =
        (fileCount + 1, acc ++ [replicate n (Just fileCount)])
    f (fileCount, acc) (SpaceSeg n) =
        (fileCount, acc ++ [replicate n Nothing])

simulateCompaction :: [Maybe FileID] -> [Maybe FileID]
simulateCompaction arr = runST $ do
    let len = length arr
    v <- M.new len
    -- Initialize vector
    let toInt Nothing = -1
        toInt (Just x) = x
    mapM_ (\(i, b) -> M.write v i (toInt b)) (zip [0 ..] arr)

    -- Two-pointer approach:
    let go f o = do
            f' <- findFree v f o
            o' <- findOccupied v o f'
            when (f' < o') $ do
                -- move block at o' to f'
                block <- M.read v o'
                M.write v f' block
                M.write v o' (-1)
                go (f' + 1) (o' - 1)

    go 0 (len - 1)

    -- Convert back
    let toMaybe x = if x == -1 then Nothing else Just x
    map toMaybe <$> mapM (M.read v) [0 .. len - 1]
  where
    findFree :: (PrimMonad m) => M.MVector (PrimState m) Int -> Int -> Int -> m Int
    findFree v f o = do
        let len = M.length v
        let loop i =
                if i > o || i >= len
                    then return i
                    else do
                        val <- M.read v i
                        if val == (-1)
                            then return i
                            else loop (i + 1)
        loop f

    findOccupied :: (PrimMonad m) => M.MVector (PrimState m) Int -> Int -> Int -> m Int
    findOccupied v o f = do
        let loop i =
                if i < f || i < 0
                    then return i
                    else do
                        val <- M.read v i
                        if val == (-1)
                            then loop (i - 1)
                            else return i
        loop o

checksum :: [Maybe FileID] -> Int
checksum arr = sum [i * fid | (i, Just fid) <- zip [0 ..] arr]

part1 :: String -> IO ()
part1 = withInput $ \inputStr -> do
    let input = T.strip (T.pack inputStr)
    let segs = parseDiskMap input
    let initial = expandLayout segs
    let finalLayout = simulateCompaction initial
    print (checksum finalLayout)
