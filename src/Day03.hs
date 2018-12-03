module Day03 (main) where

import Data.Text (split, pack, unpack)

type Claim  = (Int, Int, Int, Int, Int) -- id, left, top, width, height
(%)  = mod
(//) = div

parse :: String -> Claim
parse str =
    let i:l:t:w:h:[] = map (read . unpack) . tail . split (flip elem $ "#@,:x") . pack . filter (/= ' ') $ str
    in  (i, l, t, w, h)

doesClaim :: Int -> Claim -> Bool
doesClaim cell (_, left, top, width, height) =
    let row = cell // 1000
        col = cell %  1000
    in  (row >= top) && (row < top + height) && (col >= left) && (col < left + width)

claimCount :: [Claim] -> Int -> Int
claimCount claims cell = length . filter (doesClaim cell) $ claims

part1 :: [Claim] -> Int
part1 claims = length . filter id . map ((> 1) . claimCount claims) $ [0 .. 1000 * 1000 - 1]
    
part2 :: [Claim] -> Claim
part2 claims = filterOverlaps 0 claims
    where filterOverlaps cell acc =
            let newAcc = if claimCount claims cell > 1 then filter (not . doesClaim cell) acc else acc
            in  if length newAcc == 1 then head newAcc else filterOverlaps (cell + 1) newAcc

main :: IO ()
main = do
    input <- map parse . lines <$> readFile "input/03.txt"
    print $ part1 input
    print $ part2 input