module Main where

-- Part 1

import Data.List (nub)

data Dir = U | R | D | L deriving (Show, Eq)

turnRight :: Dir -> Dir
turnRight U = R
turnRight R = D
turnRight D = L
turnRight L = U

delta :: Dir -> (Int, Int)
delta U = (-1,0)
delta R = (0,1)
delta D = (1,0)
delta L = (0,-1)

parseInput :: [String] -> ([String], (Int, Int, Dir))
parseInput grid =
    case findGuard grid of
      Just s -> (grid, s)
      Nothing -> error "No guard found!"

findGuard :: [String] -> Maybe (Int,Int,Dir)
findGuard grid = go 0
  where
    h = length grid
    w = if null grid then 0 else length (head grid)
    go r
      | r >= h = Nothing
      | otherwise = case findInRow r 0 of
                      Just d -> Just d
                      Nothing -> go (r+1)
    findInRow r c
      | c >= w = Nothing
      | otherwise = 
          let ch = (grid !! r) !! c
          in case charDir ch of
               Just dir -> Just (r,c,dir)
               Nothing  -> findInRow r (c+1)

charDir :: Char -> Maybe Dir
charDir '^' = Just U
charDir '>' = Just R
charDir 'v' = Just D
charDir '<' = Just L
charDir _   = Nothing

outOfBounds :: [String] -> (Int,Int) -> Bool
outOfBounds g (r,c) = r < 0 || c < 0 || r >= length g || c >= length (head g)

isObstacle :: [String] -> (Int,Int) -> Bool
isObstacle g (r,c)
    | outOfBounds g (r,c) = False
    | otherwise = (g !! r) !! c == '#'

simulate :: [String] -> (Int,Int,Dir) -> [(Int,Int)]
simulate grid (sr,sc,sd) = reverse (go (sr,sc) sd [(sr,sc)])
  where
    go pos dir visited =
      let (dr,dc) = delta dir
          frontPos = (fst pos + dr, snd pos + dc)
      in if isObstacle grid frontPos
         then 
             -- Turn right
             go pos (turnRight dir) visited
         else 
             -- Move forward
             let newPos = frontPos
             in if outOfBounds grid newPos
                then visited
                else go newPos dir (newPos:visited)


part1 :: IO ()
part1 = do
    input <- lines <$> readFile "input.txt"
    let (grid, startState) = parseInput input
    let visited = simulate grid startState
    print (length (nub visited))

main :: IO ()
main = part1
