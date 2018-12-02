module Day02 (main) where

import Data.List (group, sort, (\\), intersect)

part1 :: [String] -> Int
part1 strs = (length . filter (count 2) $ strs) * (length . filter (count 3) $ strs)
    where count n = any (== n) . map length . group . sort

part2 :: [String] -> String
part2 strs = head [xs `intersect` ys | xs <- strs, ys <- strs, dist xs ys == 1]
    where dist = (length .) . (filter id .) . zipWith (/=)

main :: IO ()
main = do
    input <- lines <$> readFile "input/02.txt"
    print    $ part1 input
    putStrLn $ part2 input