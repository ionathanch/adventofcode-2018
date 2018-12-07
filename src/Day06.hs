module Day06 (main) where

import Data.List (maximumBy, sortBy)
import Data.Ord (comparing)
import Data.Function (on)
import Data.Map (fromListWith, toAscList)
import Data.Set (fromList, toList)

-- given a list of elements, return pairs of the element with its frequency
-- if two elements have the same frequency, the elements are sorted in ascending order
getElemFreqs :: Ord a => [a] -> [(a, Int)]
getElemFreqs = sortBy (flip $ comparing snd) . toAscList . fromListWith (+) . (flip zip) (repeat 1)

part1 :: [(Int, Int)] -> Int
part1 coords =
    let maxrow = fst $ maximumBy (comparing fst) coords
        maxcol = snd $ maximumBy (comparing snd) coords
        grid   = map closest [(r, c) | r <- [1..maxrow], c <- [1..maxcol]]
        edges  = toList . fromList . map closest . concat $ [[(r, 1), (1, c), (r, maxcol), (maxrow, c)] | r <- [1..maxrow], c <- [1..maxcol]]
    in  snd . head . filter ((`notElem` edges) . fst) . getElemFreqs $ grid
    where
        closest coord =
            let (car:cadr:_) = sortBy (comparing (manhattan coord)) coords
            in  if   ((==) `on` manhattan coord) car cadr
                then ((-1), (-1))
                else car
        manhattan (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2)

part2 :: [(Int, Int)] -> Int
part2 coords =
    let maxrow = fst $ maximumBy (comparing fst) coords
        maxcol = snd $ maximumBy (comparing snd) coords
    in  length . filter closeBy $ [(r, c) | r <- [1..maxrow], c <- [1..maxcol]]
    where 
        closeBy coord = (sum $ map (manhattan coord) coords) < 10000
        manhattan (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2)

main :: IO ()
main = do
    input <- map (read . ('(':) . (++ [')'])) . lines <$> readFile "input/06.txt"
    print $ part1 input
    print $ part2 input