{-# LANGUAGE BangPatterns #-}

module Day09 (main) where

import Data.IntMap (IntMap, empty, insertWith)
import Data.Sequence (Seq((:<|)), fromList, (><), (<|))
import qualified Data.Sequence as S (length, drop, take)

players    = 411
lastMarble = 7105800
infix 5 %
(%) = mod

-- (scoreboard, marbles, marble, player)
type State      = (Scoreboard, Marbles, Int, Int)
type Scoreboard = IntMap Int
type Marbles    = Seq Int

-- pop from the front and push in the back n marbles
spin :: Int -> Marbles -> Marbles
spin n m = S.drop n m >< S.take n m

-- pop from the back and push in the front n marbles
unspin :: Int -> Marbles -> Marbles
unspin n m = S.drop (S.length m - n) m >< S.take (S.length m - n) m

part1 :: State -> Int
part1 (scores, marbles, m, p)
    | nextMarble == lastMarble = maximum scores
    | nextMarble % 23 == 0 =
        let !(m:<|nextMarbles) = unspin 7 marbles
            nextScores         = insertWith (+) nextPlayer (nextMarble + m) scores
        in  part1 (nextScores, nextMarbles, nextMarble, nextPlayer)
    | otherwise =
        let !nextMarbles = nextMarble <| spin 2 marbles
        in  part1 (scores, nextMarbles, nextMarble, nextPlayer)
    where nextMarble = m + 1
          nextPlayer = (p % players) + 1

main :: IO ()
main = do
    print $ part1 (empty, fromList [1, 0], 1, 1)