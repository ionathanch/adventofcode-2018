module Day13 (main) where

import Prelude hiding (Either(..))
import Data.List (sortBy, delete)
import Data.Ord (comparing)
import Data.Function (on)
import Data.Matrix (Matrix, fromLists, setElem, (!))

type State = (Grid, [Cart])
data Cart = 
    Cart {
        pos  :: Position,
        dir  :: Direction,
        turn :: Turn
     } deriving (Show, Eq)
data Direction = Up | Right | Down | Left     deriving (Show, Enum, Eq)
data Turn      = ToLeft | Straight | ToRight  deriving (Show, Eq)
type Position = (Int, Int) -- row, col
type Grid = Matrix Char

getCarts :: String -> [Cart]
getCarts = map (\(i, c) -> Cart (indexToPos i) (cartCharToDir c) ToLeft) . filter ((`elem` "><^v") . snd) . zip [0..] . filter (/= '\n')
    where
        indexToPos i = (1 + i `div` 150, 1 + i `mod` 150)
        cartCharToDir '>' = Right
        cartCharToDir '<' = Left
        cartCharToDir '^' = Up
        cartCharToDir 'v' = Down

replaceTrack :: Cart -> Grid -> Grid
replaceTrack (Cart pos _ _) grid =
    let trackChar = case grid ! pos of
            '>' -> '-'
            '<' -> '-'
            '^' -> '|'
            'v' -> '|'
            c   -> c
    in  setElem trackChar pos grid

parse :: String -> (Grid, [Cart])
parse input =
    let carts = getCarts input
        grid  = foldr replaceTrack (fromLists $ lines input) carts
    in  (grid, carts)

tickCart :: Grid -> Cart -> Cart
tickCart grid (Cart pos dir turn) =
    let nextPos  = getNextPos pos dir
        nextChar = grid ! nextPos
        (nextDir, nextTurn) = getNextDirTurn nextChar dir turn
    in  Cart nextPos nextDir nextTurn
    where
        getNextPos (r, c) Up    = (r - 1, c)
        getNextPos (r, c) Left  = (r, c - 1)
        getNextPos (r, c) Down  = (r + 1, c)
        getNextPos (r, c) Right = (r, c + 1)
        getNextDirTurn '+'  dir   Straight = (dir, ToRight)
        getNextDirTurn '+'  dir   ToLeft   = (toEnum . (`mod` 4) . subtract 1  . fromEnum $ dir, Straight)
        getNextDirTurn '+'  dir   toRight  = (toEnum . (`mod` 4) .       (+ 1) . fromEnum $ dir, ToLeft)
        getNextDirTurn '/'  Up    turn     = (Right, turn)
        getNextDirTurn '/'  Right turn     = (Up,    turn)
        getNextDirTurn '/'  Down  turn     = (Left,  turn)
        getNextDirTurn '/'  Left  turn     = (Down,  turn)
        getNextDirTurn '\\' Up    turn     = (Left,  turn)
        getNextDirTurn '\\' Right turn     = (Down,  turn)
        getNextDirTurn '\\' Down  turn     = (Right, turn)
        getNextDirTurn '\\' Left  turn     = (Up,    turn)
        getNextDirTurn  _   dir   turn     = (dir,   turn)

part1 :: Grid -> [Cart] -> Position
part1 grid carts =
    let nextCarts = map (tickCart grid) . sortBy (comparing pos) $ carts
    in  case findFirstCollision nextCarts carts of
            Just (r, c) -> (c - 1, r - 1)
            Nothing -> part1 grid nextCarts
    where
        -- findFirstCollision :: (current cart after tick : next carts after tick) (current cart before tick : next carts before tick)
        -- the current cart can collide with any of the next carts before tick or any of the previous carts after tick
        -- for convenience, we append the previous carts after tick onto the list of old carts before tick
        findFirstCollision :: [Cart] -> [Cart] -> Maybe Position
        findFirstCollision [] _ = Nothing
        findFirstCollision (cart:carts) (oldCart:oldCarts) =
            if   any (isColliding cart) oldCarts
            then Just $ pos cart
            else findFirstCollision carts (cart:oldCarts)
        isColliding = (==) `on` pos

part2 :: Grid -> [Cart] -> Position
part2 grid carts =
    let nextCarts = map (tickCart grid) . sortBy (comparing pos) $ carts
        oldCollidingRemoved = removeCollidingOldCarts nextCarts carts []
        newCollidingRemoved = removeCollidingNewCarts oldCollidingRemoved []
    in  case newCollidingRemoved of
            ((Cart (r, c) _ _):[]) -> (c - 1, r - 1)
            cartsLeft -> part2 grid cartsLeft
    where
        removeCollidingOldCarts [] _ prevNextCarts = prevNextCarts
        removeCollidingOldCarts (cart:carts) (oldCart:oldCarts) prevNextCarts =
            case filter (isColliding cart) oldCarts of
                (collidingCart:_) -> removeCollidingOldCarts (delete (tickCart grid collidingCart) carts) (delete collidingCart oldCarts) prevNextCarts
                [] -> removeCollidingOldCarts carts oldCarts (cart:prevNextCarts)
        removeCollidingNewCarts [] prevNextCarts = prevNextCarts
        removeCollidingNewCarts (cart:carts) prevNextCarts =
            case filter (isColliding cart) carts of
                (collidingCart:_) -> removeCollidingNewCarts (delete collidingCart carts) prevNextCarts
                [] -> removeCollidingNewCarts carts (cart:prevNextCarts)
        isColliding = (==) `on` pos

main :: IO ()
main = do
    (grid, carts) <- parse <$> readFile "input/13.txt"
    print $ part1 grid carts
    print $ part2 grid carts
