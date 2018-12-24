module Day23 (main) where

import Data.SBV

manhattan :: Num a => (a, a, a) -> (a, a, a) -> a
manhattan (x1, y1, z1) (x2, y2, z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)


-- Part 1 --

data Bot = Bot Position Radius deriving Eq
type Position = (Integer, Integer, Integer)
type Radius = Integer

instance Ord Bot where
    Bot _ rad1 <= Bot _ rad2 = rad1 <= rad2

parse :: String -> Bot
parse str =
    let pos = read $ "(" ++ (takeWhile (/= '>') . drop 5 $ str) ++ ")"
        r = read . drop 2 . last . words $ str
    in  Bot pos r

part1 :: [Bot] -> Int
part1 bots =
    let maxBot = maximum bots
    in  length $ filter (botInRange maxBot) bots
    where botInRange (Bot pos1 r) (Bot pos2 _) = manhattan pos1 pos2 <= r


-- Part 2 --

data SBot = SBot SPosition SRadius
type SPosition = (SInteger, SInteger, SInteger)
type SRadius = SInteger

botToSBot :: Bot -> SBot
botToSBot (Bot (x, y, z) r) = SBot (literal x, literal y, literal z) (literal r)

problem :: [SBot] -> Goal
problem bots = do
    [x, y, z] <- sIntegers ["x", "y", "z"]
    maximize "bots" $ (inRangeOfBots (x, y, z) bots      :: SInteger)
    minimize "dist" $ (manhattan     (x, y, z) (0, 0, 0) :: SInteger)
    where
        inRangeOfBots pos = sum . map (oneIf . inRangeOfBot pos)
        inRangeOfBot  pos (SBot bPos r) = manhattan bPos pos .<= r

part2 :: [SBot] -> IO OptimizeResult
part2 = optimize Lexicographic . problem


-- main --

main :: IO ()
main = do
    bots <- map parse . lines <$> readFile "input/23.txt"
    let sbots = map botToSBot bots
    print $ part1 bots
    part2 sbots >>= print
