{-# LANGUAGE TupleSections #-}

module Day17_UArray (main) where

import Prelude hiding (lookup)
import Data.Foldable (maximumBy, minimumBy)
import Data.Ord (comparing)
import Data.Ix (range)
import Data.Array.Unboxed (UArray, array, (//), (!), elems, assocs)

-- Horz (left, right) y | Vert x (top, bottom)
type Grid = UArray Coordinate Char
type Coordinate = (Int, Int)

parse :: String -> [Coordinate]
parse str =
    let fstAxis:'=':valString   = takeWhile (/= ',') str
        sndAxis:'=':rangeString = reverse . takeWhile (/= ' ') . reverse $ str
        val = read valString
        (min, max) = (read $ (takeWhile (/= '.') rangeString), (read . reverse . takeWhile (/= '.') . reverse $ rangeString))
    in  if fstAxis == 'x' then map (val,) [min..max] else map (,val) [min..max]

-- returns minimum y, maximum y, and grid
createGrid :: [Coordinate] -> (Int, Int, Grid)
createGrid veins =
    let minX = fst $ minimumBy (comparing fst) veins
        maxX = fst $ maximumBy (comparing fst) veins
        minY = snd $ minimumBy (comparing snd) veins
        maxY = snd $ maximumBy (comparing snd) veins
        baseGrid = array ((minX - 2, 0), (maxX + 2, maxY + 1)) . map (,'.') . range $ ((minX - 2, 0), (maxX + 2, maxY + 1))
        grid = baseGrid // (((500, 0), '|') : map (,'#') veins)
    in  (minY, maxY, grid)

flowDown :: Int -> Coordinate -> Grid -> Grid
flowDown maxY (x, y) grid = if y >= maxY then grid else
    case grid ! (x, y + 1) of
        '.' -> flowDown maxY (x, y + 1) $ grid // [((x, y + 1), '|')]
        _ -> grid

canFlowDown :: Int -> Coordinate -> Grid -> Bool
canFlowDown maxY (x, y) grid = y < maxY && '.' == grid ! (x, y + 1)

-- Bool is True if water hits a wall and is over a floor, and False otherwise
flowLeft :: Coordinate -> Grid -> (Grid, Bool)
flowLeft (x, y) grid = case grid ! (x - 1, y) of
    '#' -> (grid, True)
    _ -> case grid ! (x - 1, y + 1) of
        '#' -> flowLeft (x - 1, y) $ grid // [((x - 1, y), '|')]
        '~' -> flowLeft (x - 1, y) $ grid // [((x - 1, y), '|')]
        _ -> (grid // [((x - 1, y), '|')], False)

-- Bool is True if water hits a wall and is over a floor, and False otherwise
flowRight :: Coordinate -> Grid -> (Grid, Bool)
flowRight (x, y) grid = case grid ! (x + 1, y) of
    '#' -> (grid, True)
    _ -> case grid ! (x + 1, y + 1) of
        '#' -> flowLeft (x + 1, y) $ grid // [((x + 1, y), '|')]
        '~' -> flowLeft (x + 1, y) $ grid // [((x + 1, y), '|')]
        _ -> (grid // [((x + 1, y), '|')], False)

floodLeft :: Coordinate -> Grid -> Grid
floodLeft (x, y) grid = case grid ! (x - 1, y) of
    '#' -> grid
    _ -> floodLeft (x - 1, y) $ grid // [((x - 1, y), '~')]

floodRight :: Coordinate -> Grid -> Grid
floodRight (x, y) grid = case grid ! (x + 1, y) of
    '#' -> grid
    _ -> floodLeft (x + 1, y) $ grid // [((x + 1, y), '~')]

flood :: Coordinate -> Grid -> Grid
flood coord grid =
    let (lGrid, lWall) = flowLeft  coord grid
        (rGrid, rWall) = flowRight coord lGrid
    in  if lWall && rWall then floodLeft coord . floodRight coord $ rGrid // [(coord, '~')] else rGrid

canFlood :: Coordinate -> Grid -> Bool
canFlood (x, y) grid =
    let validSides  = (grid ! (x - 1, y), grid ! (x + 1, y)) `notElem` [('#', '|'), ('|', '#'), ('|', '|')]
        validBottom = grid ! (x, y + 1) `elem` ['#', '~']
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
    let floodable = fst . unzip . filter (\(k, v) -> v == '|' && canFlood k grid) . assocs $ grid
        flooded   = foldr flood grid floodable
        flowable  = fst . unzip . filter (\(k, v) -> v == '|' && canFlowDown maxY k flooded) . assocs $ grid
        flowed    = foldr (flowDown maxY) flooded flowable
    in  if null flowable && null floodable then grid else floodFlow maxY flowed

part1 :: Int -> Int -> Grid -> Int
part1 minY maxY grid = length . filter (\((_, y), v) -> y >= minY && y <= maxY && (v == '|' || v == '~')) . assocs . floodFlow maxY $ grid

part2 :: Int -> Int -> Grid -> Int
part2 minY maxY grid = length . filter (\((_, y), v) -> y >= minY && y <= maxY && v == '~') . assocs . floodFlow maxY $ grid

main :: IO ()
main = do
    input <- readFile "input/17.txt"
    let (minY, maxY, grid) = createGrid . concat . map parse . lines $ input
    print $ (minY, maxY)
    print $ part1 minY maxY grid
    print $ part2 minY maxY grid
