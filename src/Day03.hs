module Day03 (main) where

import Data.Text (empty, split, pack, unpack)
import Data.IntMap (IntMap, fromListWith, size, notMember)
import qualified Data.IntMap as M (filter)

type Claim = (Int, [Int])

parse :: String -> Claim
parse str =
    let i:l:t:w:h:[] = map (read . unpack) . filter (/= empty) . split (flip elem $ " #@,:x") . pack $ str
    in  (i, [r * 1000 + c | r <- [t .. t + h - 1], c <- [l .. l + w - 1]])

overlapMap :: [Claim] -> IntMap Int
overlapMap = M.filter (> 1) . fromListWith (+) . (flip zip) (repeat 1) . concat . map snd

part1 :: [Claim] -> Int
part1 = size . overlapMap

part2 :: [Claim] -> Int
part2 claims = fst . head . filter (all (flip notMember $ overlapMap claims) . snd) $ claims

main :: IO ()
main = do
    input <- map parse . lines <$> readFile "input/03.txt"
    print $ part1 input
    print $ part2 input