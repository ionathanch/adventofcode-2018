module Day11 (main) where

import Data.Char (digitToInt)
import Data.List (maximumBy)
import Data.Ord (comparing)

indexToCoord :: Int -> (Int, Int)
indexToCoord i = (1 + i `div` 300, 1 + i `mod` 300)

powerLevel :: (Int, Int) -> Int
powerLevel (x, y) = (subtract 5) . digitToInt . (!! 2) . reverse . show $ ((x + 10) * y + 7672) * (x + 10)

powerSquare :: Int -> (Int, Int) -> Int
powerSquare size (x, y) =
    if   x > 300 - size + 1 || y > 300 - size + 1 then -5
    else sum [powerLevel (x', y') | x' <- [x .. x + size - 1], y' <- [y .. y + size - 1]]

part1 :: (Int, Int)
part1 = indexToCoord . fst . maximumBy (comparing snd) . zip [0..] . map (powerSquare 3 . indexToCoord) $ [0 .. 300 * 300]

main :: IO ()
main = do
    print part1