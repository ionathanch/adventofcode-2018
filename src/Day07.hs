module Day07 (main) where

import Data.List (partition)
import Data.Map (Map, fromListWith, fromList, findMin, findMax, (!), adjust, mapWithKey)
import qualified Data.Map as M (union, null, delete, empty, filter)
import Data.Set (Set, singleton, member, toList)
import qualified Data.Set as S (union, null, delete, empty)
import Data.Tuple.Extra (second)

-- Deps = Task => Dependencies
type Deps = Map Char (Set Char)

-- Worker = (Task, Time)
type Worker = (Char, Int)
emptyWorker = (' ', 0)

parse :: String -> (Char, Char)
parse str = (str !! 36, str !! 5)

dependencies :: [(Char, Char)] -> Deps
dependencies ds =
    let dependents   = fromListWith S.union . map (second singleton) $ ds
        independents = fromList . (flip zip) (repeat S.empty) . snd . unzip $ ds
    in M.union dependents independents

findRoot :: Deps -> Char
findRoot deps =
    let startChar = findMinKeyBy S.null deps
    in  crawl startChar
    where
        findMinKeyBy f = fst . findMin . M.filter f
        crawl c
            | M.null . M.filter (member c) $ deps = c
            | otherwise = crawl (findMinKeyBy (member c) deps)

transReduc :: Deps -> Deps
transReduc deps =
    let root = findRoot deps
    in  reducChildren root deps
    where
        reducChildren :: Char -> Deps -> Deps
        reducChildren node d =
            let cartProdChildren = [(t, f) | t <- toList (d ! node), f <- toList (d ! node)]
                d' = foldr (\(to, from) -> adjust (S.delete to) node) d . filter (uncurry $ pathToFrom d) $ cartProdChildren -- (flip adjust node . S.delete . fst) if you really wanted to
            in  foldr reducChildren d' (d' ! node)
        pathToFrom d to from =
            let fromChildren = d ! from
            in  member to fromChildren || any (pathToFrom d to) fromChildren

graphVizzify :: Deps -> String
graphVizzify = unlines . (["digraph G { rankdir=LR"] ++) . (++ ["}"]) . foldr1 (++) . mapWithKey (\k v -> map (((k : " -> ") ++) . pure) (toList v))

part1 :: Deps -> String -> String
part1 deps str
    | M.null deps = reverse str
    | otherwise =
        let available = fst . findMin . M.filter S.null $ deps
        in  part1 (fmap (S.delete available) (M.delete available deps)) (available:str)

part2 :: Deps -> [Worker] -> Int -> Int
part2 deps workers time
    | M.null deps = time + (maximum . snd . unzip $ workers)
    | otherwise =
        let elapsedTime            = minOrDefault 0 . filter (/= 0) . snd . unzip $ workers
            (done, working)        = partition ((== 0) . snd) . map (second $ max 0 . subtract elapsedTime) $ workers
            clearedDeps            = foldr (fmap . S.delete) deps . map fst $ done
            (newWorkers, newDeps)  = foldr assign (working, clearedDeps) $ [1 .. length done]
        in  part2 newDeps newWorkers (time + elapsedTime)
        where
            minOrDefault n ns = if null ns then n else minimum ns
            len task = fromEnum task - fromEnum 'A' + 61
            assign :: Int -> ([Worker], Deps) -> ([Worker], Deps)
            assign _ (w, d)
                | M.null $ M.filter S.null d = ((emptyWorker:w), d)
                | otherwise =
                    let task = fst . findMax . M.filter S.null $ d
                    in  (((task, len task):w), (M.delete task d))

main :: IO ()
main = do
    input <- transReduc . dependencies . map parse . lines <$> readFile "input/07.txt"
    putStrLn $ graphVizzify input
    putStrLn $ part1 input ""
    print    $ part2 input (replicate 5 emptyWorker) 0
