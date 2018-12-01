module Day01 (main) where

import Data.Set (empty, insert, member)

freqChangeList :: String -> [Int]
freqChangeList = map (read . dropWhile (== '+')) . lines

totalFrequency :: [Int] -> Int
totalFrequency = foldr1 (+)

firstFreqTwice :: [Int] -> Int
firstFreqTwice changeList =
    let freqs = scanl1 (+) $ cycle changeList
    in  dupl freqs empty
    where dupl (f:fs) s =
            if   member f s
            then f
            else dupl fs (insert f s)

main :: IO ()
main = do
    input <- readFile "input/01.txt"
    let changeList = freqChangeList input
    print $ totalFrequency changeList
    print $ firstFreqTwice changeList
