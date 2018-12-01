module Day01 (main) where

import Data.IntSet (empty, insert, member)

parseChanges :: String -> [Int]
parseChanges = map (read . dropWhile (== '+')) . lines

part1 :: [Int] -> Int
part1 = sum

part2 :: [Int] -> Int
part2 = dupl empty . scanl1 (+) . cycle
    where dupl s (f:fs)
            | member f s = f
            | otherwise  = dupl (insert f s) fs

main :: IO ()
main = do
    changes <- parseChanges <$> readFile "input/01.txt"
    print $ part1 changes
    print $ part2 changes
