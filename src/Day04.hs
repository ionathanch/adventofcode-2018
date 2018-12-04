module Day04 (main) where

import Data.List (sort, groupBy, sortBy)
import Data.Text (Text, isInfixOf, pack, unpack)
import Data.Char (isDigit)
import Data.IntMap (fromListWith, toList)

type Log = (Id, [Sleep])
type Id  = Int
type Sleep = [Minute]
type Minute = Int

(%) = mod

parse :: [String] -> [Log]
parse = map concatGuard . groupBy eqFst . sortBy compareFst . map (guardTimes . map unpack) . tail . splitByGuard [[]] . map pack
    where
        splitByGuard :: [[Text]] -> [Text] -> [[Text]]
        splitByGuard (g:gs) [] = reverse ((reverse g):gs)
        splitByGuard (g:gs) (e:es) =
            if   pack "Guard #" `isInfixOf` e
            then splitByGuard ([e]:(reverse g):gs) es
            else splitByGuard ((e:g):gs) es
        guardTimes :: [String] -> Log
        guardTimes (begin:logs) =
            let id = read . takeWhile isDigit . drop 26 $ begin :: Int
                times = pair . map (parseTime . take 5 . drop 12) $ logs
            in  (id, times)
        parseTime :: String -> Int
        parseTime ('2':'3':_:m1:m2:[]) = read [m1, m2]
        parseTime ('0':'0':_:m1:m2:[]) = read [m1, m2] + 60
        pair :: [Int] -> [[Int]]
        pair [] = []
        pair (s:w:xs) = ([s .. w - 1]:(pair xs))
        compareFst :: Ord a => (a, b) -> (a, b) -> Ordering
        compareFst t1 t2 = compare (fst t1) (fst t2)
        eqFst :: Eq a => (a, b) -> (a, b) -> Bool
        eqFst t1 t2 = (fst t1) == (fst t2)
        concatGuard :: [Log] -> Log
        concatGuard logs =
            let id = fst . head $ logs
            in  (id, concat . map snd $ logs)

part1 :: [Log] -> Int
part1 logs =
    let (id, _) = head . sortBy compareSndDesc . map (\(id, sleeps) -> (id, length . concat $ sleeps)) $ logs
        (_, sleeps) = head . filter ((== id) . fst) $ logs
        minute = (% 60) . fst . head . sortBy compareSndDesc . toList . fromListWith (+) $ zip (concat sleeps) (repeat 1)
    in  id * minute
    where
        compareSndDesc :: Ord b => (a, b) -> (a, b) -> Ordering
        compareSndDesc t1 t2 = (flip compare) (snd t1) (snd t2)

main :: IO ()
main = do
    input <- parse . sort . lines <$> readFile "input/04.txt"
    print $ part1 input
