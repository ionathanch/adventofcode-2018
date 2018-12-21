module Day21 (main) where

import Data.Bits ((.|.), (.&.), shiftR)
import Data.Set (empty, member, insert)

loop :: Int -> [Int]
loop d =
    let d' = innerLoop (d .|. 0x10000) 15028787
    in  d':(loop d')
    where
        innerLoop f d =
            let d' = math f d
                f' = shiftR f 8
            in  if f < 0x100 then d' else innerLoop f' d'
        math f d = (d + f .&. 0xFF) .&. 0xFFFFFF * 65899 .&. 0xFFFFFF

part1 :: Int
part1 = head $ loop 0

part2 :: Int
part2 =
    let vals = loop 0
    in  prevRepeat (head vals) (tail vals) empty
    where
        prevRepeat p (v:vals) set = if member v set then p else prevRepeat v vals (insert v set)

main :: IO ()
main = do
    print $ part1
    print $ part2
