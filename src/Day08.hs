module Day08 (main) where

import Data.Tree (Tree(Node), rootLabel, subForest)

type Metadata = [Int]

parseTree :: ([Tree Metadata], [Int]) -> ([Tree Metadata], [Int])
parseTree (nodes, (c:m:input)) =
    let (children, remaining) = iterate parseTree ([], input) !! c
    in  (Node (take m remaining) (reverse children) : nodes, drop m remaining)

part1 :: Tree Metadata -> Int
part1 = sum . fmap sum

part2 :: Tree Metadata -> Int
part2 tree =
    if   null (subForest tree) then sum (rootLabel tree)
    else sum . map (part2 . (subForest tree !!) . subtract 1) . filter (<= length (subForest tree)) $ rootLabel tree

main :: IO ()
main = do
    input <- map read . words <$> readFile "input/08.txt"
    let tree = head . fst $ parseTree ([], input)
    print $ part1 tree
    print $ part2 tree