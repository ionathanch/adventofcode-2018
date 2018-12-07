module Day07 (main) where

import Data.Map (Map, fromListWith, fromList, findMin)
import qualified Data.Map as M (union, null, filter, delete)
import Data.Set (Set, singleton, empty)
import qualified Data.Set as S (union, null, delete)
import Data.Tuple.Extra (second)

parse :: String -> (Char, Char)
parse str = (str !! 36, str !! 5)

dependencies :: [(Char, Char)] -> Map Char (Set Char)
dependencies ds =
    let dependents   = fromListWith S.union . map (second singleton) $ ds
        independents = fromList . (flip zip) (repeat empty) . snd . unzip $ ds
    in M.union dependents independents

part1 :: Map Char (Set Char) -> String -> String
part1 deps str
    | M.null deps = reverse str
    | otherwise =
        let available = fst . findMin . M.filter S.null $ deps
        in  part1 (fmap (S.delete available) (M.delete available deps)) (available:str)

main :: IO ()
main = do
    input <- map parse . lines <$> readFile "input/07.txt"
    putStrLn $ part1 (dependencies input) ""