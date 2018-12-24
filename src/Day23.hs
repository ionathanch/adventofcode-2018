module Day23 (main) where

data Bot = Bot Position Radius deriving (Eq)
type Position = (Int, Int, Int)
type Radius = Int

instance Ord Bot where
    Bot _ rad1 <= Bot _ rad2 = rad1 <= rad2

parse :: String -> Bot
parse str =
    let pos = takeWhile (/= '>') . drop 5 $ str
        rad = drop 2 . last . words $ str
    in  Bot (read $ "(" ++ pos ++ ")") (read rad)

manhattan :: Position -> Position -> Int
manhattan (x1, y1, z1) (x2, y2, z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

-- botInRange b1 b2 = is b2 within the range of b1?
botInRange :: Bot -> Bot -> Bool
botInRange (Bot pos1 radius) (Bot pos2 _) = manhattan pos1 pos2 <= radius

-- inRange pos bot = is pos within the range of bot?
inRange :: Bot -> Position -> Bool
inRange (Bot bPos radius) pos = manhattan bPos pos <= radius

part1 :: [Bot] -> Int
part1 bots =
    let maxBot = maximum bots
    in  length $ filter (botInRange maxBot) bots

main :: IO ()
main = do
    bots <- map parse . lines <$> readFile "input/23.txt"
    print $ part1 bots
