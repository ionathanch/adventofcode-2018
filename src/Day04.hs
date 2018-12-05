module Day04 (main) where

import Data.List (sort, sortBy)
import Data.Ord (comparing)
import Data.Char (isDigit)
import Data.Text (Text, isInfixOf, pack, unpack, split)
import qualified Data.Text as T (lines)
import Data.IntMap (IntMap, fromListWith, toAscList, (!))
import qualified Data.IntMap as M (filter)

-- Logs :: guard id => [minutes asleep]
type Logs = IntMap [Int]

(%) = mod

getKeyWithMaxValBy :: Ord b => (a -> b) -> IntMap a -> (Int, a)
getKeyWithMaxValBy f = head . sortBy (flip $ comparing (f . snd)) . toAscList

getIntWithMaxFreq :: [Int] -> (Int, Int)
getIntWithMaxFreq = getKeyWithMaxValBy id . fromListWith (+) . (flip zip) (repeat 1)

parse :: [String] -> Logs
parse input = 
    let partitioned :: [[String]] -- list of guards' lists of logs
        partitioned = tail . map (map unpack . filter (not . isInfixOf (pack "Guard")) . T.lines) . split (== '#') . pack . unlines $ input
    in  fromListWith (++) . map guardTimes $ partitioned
    where
        guardTimes :: [String] -> (Int, [Int])
        guardTimes (begin:logs) =
            let id = read . takeWhile isDigit $ begin
                times = minutes . map (parseTime . take 5 . drop 12) $ logs
            in  (id, times)
        parseTime :: String -> Int
        parseTime ('2':'3':_:m1:m2:[]) = read [m1, m2]
        parseTime ('0':'0':_:m1:m2:[]) = read [m1, m2] + 60
        minutes :: [Int] -> [Int]
        minutes [] = []
        minutes (s:w:xs) = [s .. w - 1] ++ (minutes xs)

part1 :: Logs -> Int
part1 logs =
    let guard = fst . getKeyWithMaxValBy id . fmap length $ logs
        minute = (% 60) . fst . getIntWithMaxFreq $ logs ! guard
    in  guard * minute

part2 :: Logs -> Int
part2 logs =
    let idMinuteFreq :: IntMap (Int, Int) -- :: guard id => (most frequently asleep minute, frequency)
        idMinuteFreq = fmap getIntWithMaxFreq . M.filter (not . null) $ logs
        guard = fst . getKeyWithMaxValBy snd $ idMinuteFreq
        minute = (% 60) . fst $ idMinuteFreq ! guard
    in  guard * minute

main :: IO ()
main = do
    input <- parse . sort . lines <$> readFile "input/04.txt"
    print $ part1 input
    print $ part2 input
