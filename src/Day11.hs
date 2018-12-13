module Day11 (main) where

import Data.Char (digitToInt)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Tuple (swap)
import Data.Matrix (Matrix, matrix, toLists, fromLists, getCol)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V (scanl1, zipWith3)

powerLevel :: (Int, Int) -> Int
powerLevel (x, y) = (subtract 5) . digitToInt . (!! 2) . reverse . show $ ((x + 10) * y + 7672) * (x + 10)

-- maxSubvectorSum :: size -> vector -> (value, y-coord)
maxSubvectorSum :: Int -> Vector Int -> (Int, Int)
maxSubvectorSum size v =
    let accuml = V.scanl1 (+) v
        summed = map (\i -> accuml ! (i + size - 1) - accuml ! i + v ! i) [0 .. 300 - size]
        zipped = zip summed [1..]
    in  maximumBy (comparing fst) zipped

-- maxSubsquareSum :: grid -> grid with scanl'd rows -> (left, right) -> (subsquare sum, (x, y, size))
maxSubsquareSum :: Matrix Int -> Matrix Int -> (Int, Int) -> (Int, (Int, Int, Int))
maxSubsquareSum grid accRow (left, right) =
    let vec    = V.zipWith3 (\accR accL gridL -> accR - accL + gridL) (getCol right accRow) (getCol left accRow) (getCol left grid)
        size   = right - left + 1
        (v, y) = maxSubvectorSum size vec
    in  (v, (left, y, size))

-- part2 :: (x, y, size)
part2 :: (Int, Int, Int)
part2 =
    let grid     = matrix 300 300 (powerLevel . swap)
        accRow   = fromLists . map (scanl1 (+)) . toLists $ grid
        colRange = [(left, right) | left <- [1..300], right <- [left..300]] -- part1: [(left, left + 2) | left <- [1..298]]
    in  snd . maximumBy (comparing fst) . map (maxSubsquareSum grid accRow) $ colRange

main :: IO ()
main = print part2