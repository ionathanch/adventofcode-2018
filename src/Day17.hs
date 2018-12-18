{-# LANGUAGE TupleSections #-}

module Day17 (main) where

import Prelude hiding (lookup)
import Data.Foldable (maximumBy, minimumBy)
import Data.Ord (comparing)
import Data.Map (Map, fromList, insert, lookup, filterWithKey, keys, size)
import Debug.Trace (traceShow, traceShowId)

-- Horz (left, right) y | Vert x (top, bottom)
data Vein = Horz (Int, Int) Int | Vert Int (Int, Int)
type Grid = Map Coordinate Char
type Coordinate = (Int, Int)

parse :: String -> Vein
parse str =
    let fstAxis:'=':valString   = takeWhile (/= ',') str
        sndAxis:'=':rangeString = reverse . takeWhile (/= ' ') . reverse $ str
        val   = read valString
        range = (read $ (takeWhile (/= '.') rangeString), (read . reverse . takeWhile (/= '.') . reverse $ rangeString))
    in  if fstAxis == 'x' then Vert val range else Horz range val

-- returns minimum y, maximum y, and grid
createGrid :: [Vein] -> (Int, Int, Grid)
createGrid veins =
    let minY = getMinY $ minimumBy (comparing getMinY) veins
        maxY = getMaxY $ maximumBy (comparing getMaxY) veins
        grid = foldr insertVein (fromList [((500, 0), '|')]) veins
    in  (minY, maxY, grid)
    where
        getMinY (Horz _ y) = y
        getMinY (Vert _ (top, _)) = top
        getMaxY (Horz _ y) = y
        getMaxY (Vert _ (_, bottom)) = bottom   
        insertVein (Horz (left, right) y) grid = foldr (\coord g -> insert coord '#' g) grid $ map (,y) [left..right]
        insertVein (Vert x (top, bottom)) grid = foldr (\coord g -> insert coord '#' g) grid $ map (x,) [top..bottom]

flowDown :: Int -> Coordinate -> Grid -> Grid
flowDown maxY (x, y) grid = if y >= maxY then grid else
    case lookup (x, y + 1) grid of
        Just _  -> grid
        Nothing -> flowDown maxY (x, y + 1) $ insert (x, y + 1) '|' grid

canFlowDown :: Int -> Coordinate -> Grid -> Bool
canFlowDown maxY (x, y) grid = y < maxY && Nothing == lookup (x, y + 1) grid

-- Bool is True if water hits a wall and is over a floor, and False otherwise
flowLeft :: Coordinate -> Grid -> (Grid, Bool)
flowLeft (x, y) grid = case lookup (x - 1, y) grid of
    Just '#' -> (grid, True)
    _ -> case lookup (x - 1, y + 1) grid of
        Just '#' -> flowLeft (x - 1, y) $ insert (x - 1, y) '|' grid
        Just '~' -> flowLeft (x - 1, y) $ insert (x - 1, y) '|' grid
        _ -> (insert (x - 1, y) '|' grid, False)

-- Bool is True if water hits a wall and is over a floor, and False otherwise
flowRight :: Coordinate -> Grid -> (Grid, Bool)
flowRight (x, y) grid = case lookup (x + 1, y) grid of
    Just '#' -> (grid, True)
    _ -> case lookup (x + 1, y + 1) grid of
        Just '#' -> flowRight (x + 1, y) $ insert (x + 1, y) '|' grid
        Just '~' -> flowRight (x + 1, y) $ insert (x + 1, y) '|' grid
        _ -> (insert (x + 1, y) '|' grid, False)

floodLeft :: Coordinate -> Grid -> Grid
floodLeft (x, y) grid = case lookup (x - 1, y) grid of
    Just '#' -> grid
    _ -> floodLeft (x - 1, y) $ insert (x - 1, y) '~' grid

floodRight :: Coordinate -> Grid -> Grid
floodRight (x, y) grid = case lookup (x + 1, y) grid of
    Just '#' -> grid
    _ -> floodRight (x + 1, y) $ insert (x + 1, y) '~' grid

flood :: Coordinate -> Grid -> Grid
flood coord grid =
    let (lGrid, lWall) = flowLeft  coord grid
        (rGrid, rWall) = flowRight coord lGrid
    in  if lWall && rWall then floodLeft coord . floodRight coord $ insert coord '~' rGrid else rGrid

canFlood :: Coordinate -> Grid -> Bool
canFlood (x, y) grid =
    let validSides  = (lookup (x - 1, y) grid, lookup (x + 1, y) grid) `notElem` [(Just '#', Just '|'), (Just '|', Just '#'), (Just '|', Just '|')]
        validBottom = lookup (x, y + 1) grid `elem` [Just '#', Just '~']
    in  validSides && validBottom

{-
A few edge cases:
    * Flowing water with more flowing water on one side (. | | or | | .) could flow into the gap
        . | |      | | |
        . # |  =>  | # |
        . # |      | # |
    * Flowing water surrounded by walls should become stagnant
        # | #      # ~ #
        # | #  =>  # ~ #
        # # #      # # #
    * But flowing water between a wall and more flowing water never changes
      and neither does flowing water between more flowing water
        # | | |
        # # # |
-}

floodFlow :: Int -> Grid -> Grid
floodFlow maxY grid =
    let floodable = keys $ filterWithKey (\k v -> v == '|' && canFlood k grid) grid
        flooded   = foldr flood grid floodable
        flowable  = keys $ filterWithKey (\k v -> v == '|' && canFlowDown maxY k flooded) flooded
        flowed    = foldr (flowDown maxY) flooded flowable
    in  if null flowable && null floodable then grid else floodFlow maxY flowed

part1 :: Int -> Int -> Grid -> Int
part1 minY maxY grid = size . filterWithKey (\(_, y) v -> y >= minY && y <= maxY && (v == '|' || v == '~')) . floodFlow maxY $ grid

part2 :: Int -> Int -> Grid -> Int
part2 minY maxY grid = size . filterWithKey (\(_, y) v -> y >= minY && y <= maxY && v == '~') . floodFlow maxY $ grid

main :: IO ()
main = do
    (minY, maxY, grid) <- createGrid . map parse . lines <$> readFile "input/17.txt"
    print $ part1 minY maxY grid
    print $ part2 minY maxY grid
