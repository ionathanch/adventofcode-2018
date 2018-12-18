module Day18 (main) where

import Data.Maybe (catMaybes)
import Data.Matrix (Matrix, fromLists, nrows, ncols, (!), safeGet, setElem)

type Landscape = Matrix Char
type Coordinate = (Int, Int) -- row, col

stepCoord :: Landscape -> Coordinate -> Landscape -> Landscape
stepCoord oldscape (r, c) newscape =
    let neighbours = catMaybes $ map (flip (uncurry safeGet) oldscape) [(r', c') | r' <- [r-1 .. r+1], c' <- [c-1 .. c+1], (r', c') /= (r, c)]
        newAcre = case oldscape ! (r, c) of
            '.' -> if (length $ filter (== '|') neighbours) >= 3 then '|' else '.'
            '|' -> if (length $ filter (== '#') neighbours) >= 3 then '#' else '|'
            '#' -> if '|' `elem` neighbours && '#' `elem` neighbours then '#' else '.'
    in  setElem newAcre (r, c) newscape

stepScape :: Landscape -> Landscape
stepScape oldscape = foldr (stepCoord oldscape) oldscape [(r, c) | r <- [1 .. nrows oldscape], c <- [1 .. ncols oldscape]]

part1 :: Landscape -> Int
part1 scape =
    let newscape = iterate stepScape scape !! 10
        woodedCount     = sum $ fmap (fromEnum . (== '|')) newscape
        lumberyardCount = sum $ fmap (fromEnum . (== '#')) newscape
    in  woodedCount * lumberyardCount

part2 :: Landscape -> Int
part2 scape =
    let resVals = map getResVal $ iterate stepScape scape
        (i, i') = (461, 489) -- cycleIndices (drop 1 resVals) (drop 2 resVals) 1 2
        cyclePos = (1000000000 - i) `mod` (i' - i) + i
    in  getResVal $ iterate stepScape scape !! cyclePos -- resVals !! cyclePos
    where
        getResVal scape =
            let woodedCount     = sum $ fmap (fromEnum . (== '|')) scape
                lumberyardCount = sum $ fmap (fromEnum . (== '#')) scape
            in  woodedCount * lumberyardCount
        -- theoretically this works but it was just faster to print out the first 500 values
        cycleIndices (t:ts) (h:_:hs) ti hi =
            if t == h then (ti, hi) else cycleIndices ts hs (ti + 1) (hi + 2)

main :: IO ()
main = do
    input <- fromLists . lines <$> readFile "input/18.txt"
    print $ part1 input
    print $ part2 input