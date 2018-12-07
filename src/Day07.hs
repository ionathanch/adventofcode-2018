module Day07 (main) where

import Data.List (partition)
import Data.Map (Map, fromListWith, fromList, findMin, findMax)
import qualified Data.Map as M (union, null, delete, empty, filter)
import Data.Set (Set, singleton)
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
    input <- dependencies . map parse . lines <$> readFile "input/07.txt"
    putStrLn $ part1 input ""
    print    $ part2 input (replicate 5 emptyWorker) 0
