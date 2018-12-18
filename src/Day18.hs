module Day18 (main) where

import Data.Maybe (catMaybes)
import Data.Foldable (foldl')
import Data.Ix (range, inRange)
import Data.Array.Unboxed

type Landscape = UArray Coordinate Char
type Coordinate = (Int, Int) -- row, col

parse :: String -> Landscape
parse str =
    let nestedList = lines str
        maxIx = (length nestedList, length $ head nestedList)
    in  listArray ((1, 1), maxIx) $ concat nestedList

stepCoord :: Landscape -> Landscape -> Coordinate -> Landscape
stepCoord oldscape newscape coord@(r, c) =
    let neighbours = catMaybes . map (safeGet oldscape) . filter (/= coord) $ range ((r - 1, c - 1), (r + 1, c + 1))
        newAcre = case oldscape ! coord of
            '.' -> if (length $ filter (== '|') neighbours) >= 3 then '|' else '.'
            '|' -> if (length $ filter (== '#') neighbours) >= 3 then '#' else '|'
            '#' -> if '|' `elem` neighbours && '#' `elem` neighbours then '#' else '.'
    in  newscape // [(coord, newAcre)]
    where
        safeGet :: Landscape -> Coordinate -> Maybe Char
        safeGet scape coord = if inRange (bounds scape) coord then Just $ scape ! coord else Nothing

stepScape :: Landscape -> Landscape
stepScape oldscape = foldl' (stepCoord oldscape) oldscape $ indices oldscape

getResVal :: Landscape -> Int
getResVal scape =
    let woodedCount     = length . filter (== '|') . elems $ scape
        lumberyardCount = length . filter (== '#') . elems $ scape
    in  woodedCount * lumberyardCount

part1 :: Landscape -> Int
part1 scape = getResVal $ iterate stepScape scape !! 10

part2 :: Landscape -> Int
part2 scape =
    let resVals = map getResVal $ iterate stepScape scape
        (i1, i2) = tortoiseHare (drop 1 resVals) (drop 2 resVals) 1 2
        pos = (1000000000 - i1) `mod` (i2 - i1) + i1
    in  resVals !! pos
    where
        tortoiseHare (t:ts) (h:_:hs) ti hi =
            if t == h then (ti, nextOcc t ts (ti + 1)) else tortoiseHare ts hs (ti + 1) (hi + 2)
        nextOcc v (r:rs) n =
            if v == r then n else nextOcc v rs (n + 1)

main :: IO ()
main = do
    input <- parse <$> readFile "input/18.txt"
    print $ part1 input
    print $ part2 input