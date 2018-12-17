module Day14 (main) where

import Data.Char (digitToInt, intToDigit)
import Data.List (tails, isPrefixOf)
import Data.Sequence (Seq(..), fromList, index, (><))

type Scores = Seq Int
type Elves  = (Int, Int) -- indices of first and second elf

(!) = index

input = 793061
inputList = [7, 9, 3, 0, 6, 1]
initialScores = [3, 7]
initialElves  = (0, 1)

getScores :: [Int]
getScores = initialScores ++ getNextScores initialElves (fromList initialScores)
    where getNextScores (e1, e2) scores =
            let newScores  = map digitToInt . show $ scores ! e1 + scores ! e2
                nextScores = scores >< fromList newScores
                nextElves  = ((e1 + scores ! e1 + 1) `mod` length nextScores, (e2 + scores ! e2 + 1) `mod` length nextScores)
            in  newScores ++ getNextScores nextElves nextScores

part1 :: String
part1 = map intToDigit . take 10 . drop input $ getScores

part2 :: Int
part2 = length . takeWhile (not . (inputList `isPrefixOf`)) . tails $ getScores

main :: IO ()
main = do
    print part1
    print part2