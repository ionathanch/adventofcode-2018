{-# LANGUAGE ViewPatterns #-}

module Day24 (main) where

import Data.Foldable (foldl')
import Data.List (maximumBy, sort, sortOn, delete, find)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)

type Pairs = ([Group], [Group], [(Group, Int)])

data Group = Group {
    number       :: Int,
    army         :: Army,
    units        :: Int,
    hitPoints    :: Int,   -- of each unit
    immunities   :: [AttackType],
    weaknesses   :: [AttackType],
    attackType   :: AttackType,
    attackDamage :: Int,
    initiative   :: Int
} deriving (Eq, Ord, Show)
data Army = Immune | Infection deriving (Eq, Ord, Show)
data AttackType = Fire | Slashing | Radiation | Bludgeoning | Cold deriving (Eq, Ord, Show)

demoImmuneSystem :: [Group]
demoImmuneSystem = [
        Group 1 Immune    17   5390 []          [Radiation, Bludgeoning]    Fire        4507 2,
        Group 2 Immune    989  1274 [Fire]      [Bludgeoning, Slashing]     Slashing    25   3
    ]

demoInfection :: [Group]
demoInfection = [
        Group 1 Infection 801  4706 []          [Radiation]                 Bludgeoning 116  1,
        Group 2 Infection 4485 2961 [Radiation] [Fire, Cold]                Slashing    12   4
    ]

initImmuneSystem :: [Group]
initImmuneSystem = [
        Group  1 Immune    5711 6662     [Fire]                          [Slashing]                  Bludgeoning 9   14,
        Group  2 Immune    2108 8185     []                              [Radiation, Bludgeoning]    Slashing    36  13,
        Group  3 Immune    1590 3940     []                              []                          Cold        24  5,
        Group  4 Immune    2546 6960     []                              []                          Slashing    25  2,
        Group  5 Immune    1084 3450     [Bludgeoning]                   []                          Slashing    27  11,
        Group  6 Immune    265  8223     [Radiation, Bludgeoning, Cold]  []                          Cold        259 12,
        Group  7 Immune    6792 6242     [Slashing]                      [Bludgeoning, Radiation]    Slashing    9   18,
        Group  8 Immune    3336 12681    []                              [Slashing]                  Fire        28  6,
        Group  9 Immune    752  5272     [Slashing]                      [Bludgeoning, Radiation]    Radiation   69  4,
        Group 10 Immune    96   7266     [Fire]                          []                          Bludgeoning 738 8
    ]

initInfection :: [Group]
initInfection = [
        Group  1 Infection 1492 47899    [Cold]                          [Fire, Slashing]            Bludgeoning 56  15,
        Group  2 Infection 3065 39751    []                              [Bludgeoning, Slashing]     Slashing    20  1,
        Group  3 Infection 7971 35542    []                              [Bludgeoning, Radiation]    Bludgeoning 8   10,
        Group  4 Infection 585  5936     [Fire]                          [Cold]                      Slashing    17  17,
        Group  5 Infection 2449 37159    [Cold]                          []                          Cold        22  7,
        Group  6 Infection 8897 6420     [Bludgeoning, Slashing, Fire]   [Radiation]                 Bludgeoning 1   19,
        Group  7 Infection 329  31704    [Cold, Radiation]               [Fire]                      Bludgeoning 179 16,
        Group  8 Infection 6961 11069    []                              [Fire]                      Radiation   2   20,
        Group  9 Infection 2837 29483    []                              [Cold]                      Bludgeoning 20  9,
        Group 10 Infection 8714 7890     []                              []                          Cold        1   3
    ]

effectivePower :: Group -> Int
effectivePower g = units g * attackDamage g

-- damage :: attacking group -> defending group -> damage dealt
damage :: Group -> Group -> Int
damage a d
    | attackType a `elem` immunities d = 0
    | attackType a `elem` weaknesses d = 2 * effectivePower a
    | otherwise = effectivePower a

-- chooseTarget :: attacking group -> target groups -> target group
chooseTarget :: Group -> [Group] -> Maybe Group
chooseTarget a groups =
    let target = maximumBy (comparing (\t -> (damage a t, effectivePower t, initiative t))) groups
    in  if damage a target == 0 then Nothing else Just target

-- pair :: (immune system groups, infection groups, (attacking group, defending number)) -> attacking group -> (remaining immunes, remaining infections, new pairs)
pair :: Pairs -> Group -> Pairs
pair paired@(_, [], _) group@(army -> Immune)    = paired
pair paired@([], _, _) group@(army -> Infection) = paired
pair paired@(immune, infection, pairs) group@(army -> Immune) =
    case chooseTarget group infection of
        Just target -> (immune, delete target infection, (group, number target):pairs)
        Nothing -> paired
pair paired@(immune, infection, pairs) group@(army -> Infection) =
    case chooseTarget group immune of
        Just target -> (delete target immune, infection, (group, number target):pairs)
        Nothing -> paired

-- attack :: (immune system groups, infection groups) -> (attacking group, defending number) -> remaining (immunes, infections)
attack :: ([Group], [Group]) -> (Group, Int) -> ([Group], [Group])
attack groups@(immune, infection) (Group { number = n, army = Immune }, i) =
    fromMaybe groups $ do
        a <- find ((== n) . number) immune
        d <- find ((== i) . number) infection
        let unitsLeft     = (units d) - (damage a d) `div` (hitPoints d)
            infectionRest = delete d infection
        Just $ if unitsLeft > 0 then (immune, d { units = unitsLeft } : infectionRest) else (immune, infectionRest)
attack groups@(immune, infection) (Group { number = n, army = Infection }, i) =
    fromMaybe groups $ do
        a <- find ((== n) . number) infection
        d <- find ((== i) . number) immune
        let unitsLeft     = (units d) - (damage a d) `div` (hitPoints d)
            immuneRest    = delete d immune
        Just $ if unitsLeft > 0 then (d { units = unitsLeft } : immuneRest, infection) else (immuneRest, infection)

-- fight :: (immune system groups, infection groups) before fight -> (immune, infection) after
fight :: ([Group], [Group]) -> ([Group], [Group])
fight (immune, infection) =
    let chooseOrder = reverse . sortOn (\g -> (effectivePower g, initiative g)) $ immune ++ infection
        (_, _, pairs) = foldl' pair (immune, infection, []) chooseOrder
        attackOrder = reverse . sortOn (initiative . fst) $ pairs
    in  foldl' attack (immune, infection) attackOrder

-- getOutcome :: (immune system groups, infection groups) -> (winning army, remaining units or -1 if stalemate)
getOutcome :: ([Group], [Group]) -> (Army, Int)
getOutcome (immune, [])    = (Immune,    sum $ map units immune)
getOutcome ([], infection) = (Infection, sum $ map units infection)
getOutcome ii@(immune, infection) =
    let ii'@(immune', infection') = fight ii
    in  if   sort immune' == sort immune && sort infection' == sort infection
        then (Infection, -1) -- stalemate
        else getOutcome ii'

part1 :: ([Group], [Group]) -> Int
part1 = snd . getOutcome

part2 :: ([Group], [Group]) -> Int
part2 ii@(immune, infection) =
    let (army, n) = getOutcome ii
    in  if army == Immune then n else part2 (boost 1 immune, infection)
    where boost n = map (\g -> g { attackDamage = n + attackDamage g })

main :: IO ()
main = do
    print $ part1 (initImmuneSystem, initInfection)
    print $ part2 (initImmuneSystem, initInfection)
