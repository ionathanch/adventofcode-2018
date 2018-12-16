module Day12 (main) where

import Prelude hiding (replicate, fromList)
import Data.Bits ((.|.), shift)
import Data.List (sort)
import Data.Foldable (toList)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V (fromList)
import Data.Sequence (Seq(..), (><), fromList, takeWhileL, takeWhileR, replicate, empty)
import Debug.Trace (traceShow)

type PotState = (Int, Pots)
type Pots     = Seq Char
type Notes    = Vector Char

parse :: String -> (Notes, Pots)
parse str =
    let init:_:ns = lines str
        seq   = fromList . drop 15 $ init
        notes = V.fromList . map last . reverse . sort $ ns
    in  (notes, seq)

potsToInt :: Seq Char -> Int
potsToInt = potsToIntRec 0 5
    where
        potsToIntRec val 0 _       = val
        potsToIntRec val n (c:<|cs) = potsToIntRec (val `shift` 1 .|. fromEnum (c == '#')) (n - 1) cs

appendEmptyPots :: PotState -> PotState
appendEmptyPots potState@(lp, ps) =
    let lPots = max 0 . (5 -) . length . takeWhileL (== '.') $ ps
        rPots = max 0 . (5 -) . length . takeWhileR (== '.') $ ps
    in  (lp - lPots, replicate lPots '.' >< ps >< replicate rPots '.')

grow :: Notes -> PotState -> PotState
grow notes potState =
    let (leftPot, pots) = appendEmptyPots potState
        (_, newPots) = foldr (\_ (op@(p:<|ps), np) -> (ps, np :|> notes ! potsToInt op)) (pots, empty) [0 .. length pots - 5]
    in  (leftPot, fromList ".." >< newPots >< fromList "..")

sumPotNums :: PotState -> Int
sumPotNums (leftPot, pots) = sum . map fst . filter ((== '#') . snd) . zip [leftPot..] . toList $ pots

part1 :: Notes -> Pots -> Int
part1 notes pots = sumPotNums $ iterate (grow notes) (0, pots) !! 20

part2 :: Notes -> Pots -> Int
part2 notes pots =
    let sums  = map sumPotNums $ iterate (grow notes) (0, pots)
        diffs = zipWith (-) (tail sums) sums
        index = findRepeatIndex 1 diffs
        diff  = sums !! (index + 1) - sums !! index
    in  sums !! index + diff * (50000000000 - index)
    where findRepeatIndex n (d1:d2:ds) =
            if d1 == d2 then n else findRepeatIndex (n + 1) (d2:ds)

main :: IO ()
main = do
    (notes, pots) <- parse <$> readFile "input/12.txt"
    print $ part1 notes pots
    print $ part2 notes pots
