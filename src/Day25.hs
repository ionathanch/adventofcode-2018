{-# LANGUAGE ViewPatterns #-}

module Day25 (main) where

import Prelude hiding (null)
import Data.Foldable (foldl')
import Data.Set (Set, fromList, null, empty, insert, deleteFindMin, partition, union)

type Point = (Int, Int, Int, Int)

parse :: String -> Point
parse str = read $ "(" ++ str ++ ")"

manhattan :: Point -> Point -> Int
manhattan (t1, x1, y1, z1) (t2, x2, y2, z2) = abs (t2 - t1) + abs (x2 - x1) + abs (y2 - y1) + abs (z2 - z1)

-- constellation :: starting point -> points -> (points in same constellation, points not in same constellation)
constellation :: Point -> Set Point -> (Set Point, Set Point)
constellation p ps =
    let (near, far)  = partition ((<= 3) . manhattan p) ps
        (same, diff) = foldl' (\(n, f) p -> let (s, d) = constellation p f in (union n s, d)) (empty, far) near
    in  (insert p $ union same near, diff)

constellations :: Set Point -> [Set Point] -> [Set Point]
constellations (null -> True) cs = cs
constellations points cs =
    let (p, ps) = deleteFindMin points
        (same, diff) = constellation p ps
    in  constellations diff (same:cs)

part1 :: Set Point -> Int
part1 points = length $ constellations points []

main :: IO ()
main = do
    input <- fromList . map parse . lines <$> readFile "input/25.txt"
    print $ part1 input
