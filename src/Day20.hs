module Day20 (main) where

import Prelude hiding (null, sum)
import Data.Foldable (foldl', toList, sum)
import Control.Applicative ((<*))
import Text.Parsec (Parsec, eof, try, choice, many1, sepBy1, (<|>))
import qualified Text.Parsec as P (parse)
import Text.Parsec.Token (makeTokenParser, parens)
import Text.Parsec.Char (char, endOfLine , oneOf)
import Text.Parsec.Language (emptyDef)
import Data.Map.Strict (Map, empty, unionWith, unionsWith, fromList, insert, filterWithKey, (!))
import Data.Set (Set, singleton, null, union, unions, size, (\\))
import qualified Data.Set as S (fromList)

type Parser = Parsec String ()
data AndPath = Simple String | OrPath [Path]
type Path   = [AndPath]
type Graph = Map Coordinate (Set Coordinate)
type Coordinate = (Int, Int)

(//) = div
dirs = many1 $ oneOf "NEWS"
pars = parens $ makeTokenParser emptyDef

parser1 :: Parser Int
parser1 = char '^' >> parserRec <* char '$' <* endOfLine <* eof
    where 
        parserRec = sum <$> (many1 $ choice [length <$> dirs, pars maxSubexp])
        maxSubexp = do
            lengths <- sepBy1 (try parserRec <|> return 0) (char '|')
            return $ if any (== 0) lengths then 0 else maximum lengths

parser2 :: Parser Path
parser2 = char '^' >> parserRec <* char '$' <* endOfLine <* eof
    where
        parserRec = many1 $ choice [Simple <$> dirs, pars subexp]
        subexp    = OrPath <$> sepBy1 (try parserRec <|> (return $ [Simple ""])) (char '|')

parse :: String -> Parser a -> a
parse input parser = case P.parse parser "" input of
    Left e -> error $ show e
    Right r -> r

pathToGraph :: (Coordinate, Graph) -> Path -> (Coordinate, Graph)
pathToGraph cg [] = cg
pathToGraph cg ((Simple str):rest) = pathToGraph (foldl' addEdge cg str) rest
    where
        addEdge (coord, graph) dir =
            let newCoord = step coord dir
                newGraph = unionWith union (fromList [(coord, singleton newCoord), (newCoord, singleton coord)]) graph
            in  (newCoord, newGraph)
        step (x, y) 'N' = (x, y + 1)
        step (x, y) 'E' = (x + 1, y)
        step (x, y) 'S' = (x, y - 1)
        step (x, y) 'W' = (x - 1, y)
pathToGraph cg@(coord, graph) ((OrPath paths):rest) =
    let newGraph = unionsWith union $ map (snd . pathToGraph cg) paths
    in  pathToGraph (coord, newGraph) rest

-- bfs :: graph -> map from distances to rooms with that minimum distance
bfs :: Graph -> Map Int (Set Coordinate)
bfs graph = bfsRec (fmap (\\ initialRoom) graph) 1 initialRoom (fromList [(0, initialRoom)])
    where
        initialRoom = S.fromList [(0, 0)]
        bfsRec graph n coords distances = if null coords then distances else
            let coordsReachable = unions . map (graph !) $ toList coords
                newDistances = insert n coordsReachable distances
                newGraph = fmap (\\ coordsReachable) graph
            in  bfsRec newGraph (n + 1) coordsReachable newDistances

part2 :: Path -> Int
part2 = sum . fmap size . filterWithKey (\k v -> k >= 1000) . bfs . snd . pathToGraph ((0, 0), empty)

main :: IO ()
main = do
    input <- readFile "input/20.txt"
    print $ parse input parser1
    print . part2 $ parse input parser2
