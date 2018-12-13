module Day10 (main) where

import Data.List (sortBy, groupBy)
import Data.Ord (comparing)
import Data.Function (on)
import Data.Tuple.Extra ((***))
import Data.Matrix (matrix, toLists)

type Point = (Position, Velocity)
type Position = (Int, Int) -- ( x,  y)
type Velocity = (Int, Int) -- (dx, dy)

stepPoint :: Point -> Point
stepPoint ((x, y), (dx, dy)) = ((x + dx, y + dy), (dx, dy))

parse :: String -> Point
parse str =
    let position = "(" ++ (take 14 . drop 10 $ str) ++ ")"
        velocity = "(" ++ (take  6 . drop 36 $ str) ++ ")"
    in  (read position, read velocity)

alignedX :: Int -> [Point] -> Bool
alignedX n =
    any nearby . map (snd . unzip) . groupBy ((==) `on` fst) . sortBy (comparing fst) . fst . unzip
    where nearby ys = length ys >= n && (maximum ys) - (minimum ys) < n

toGrid :: [Point] -> String
toGrid points =
    let positions = fst . unzip $ points
        minX = minimum . fst . unzip $ positions
        minY = minimum . snd . unzip $ positions
        maxX = maximum . fst . unzip $ positions
        maxY = maximum . snd . unzip $ positions
        isPoint (y, x) = if (x + minX - 1, y + minY - 1) `elem` positions then '#' else '.'
        grid = matrix (maxY - minY + 1) (maxX - minX + 1) isPoint
    in  unlines . toLists $ grid

part1 :: Int -> [Point] -> (String, Int)
part1 sec points =
    if   alignedX 10 points
    then (toGrid $ points, sec)
    else part1 (sec + 1) $ map stepPoint points

main :: IO ()
main = do
    input <- map parse . lines <$> readFile "input/10.txt"
    let result = part1 0 input
    putStrLn $ fst result
    print $ snd result
