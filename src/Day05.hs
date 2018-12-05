module Day05 (main) where

import Data.Char (toLower)

part1 :: String -> Int
part1 = length . foldr react []
    where react c [] = [c]
          react c (u:us) = if (c /= u) && (toLower c == toLower u) then us else (c:u:us)

part2 :: String -> Int
part2 str = minimum . map (\c -> part1 . filter ((/= c) . toLower) $ str) $ ['a'..'z']

main :: IO ()
main = do
    input <- readFile "input/05.txt"
    print $ part1 input
    print $ part2 input