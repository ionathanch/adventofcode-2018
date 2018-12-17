module Day16 (main) where

import Prelude hiding (null, (!!))
import Data.Bits ((.&.), (.|.))
import Data.Foldable (all, toList, foldl')
import Data.Sequence (Seq, fromList, update, index)
import qualified Data.Map as M' (fromList, (!))
import Data.IntMap (IntMap, null, insertWith, partition, (!))
import qualified Data.IntMap as M (empty, union)
import Data.Set (Set, singleton, size, (\\))
import qualified Data.Set as S (empty, union)

-- Instr OpCode RegisterA/ImmediateA RegisterB/ImmediateB RegisterC
data Instruction = Instr Int Int Int Int deriving Show
type Registers = Seq Int
type Sample = (Instruction, Registers, Registers)
type OpsMap = IntMap (Set String)
type OpMap  = IntMap Op
type Op = Instruction -> Registers -> Registers

infixl 9 !!
(!!) = index

(!!!) = (M'.!)

initialRegisters = fromList [0, 0, 0, 0]

applyInstruction :: OpMap -> Op
applyInstruction opMap instr@(Instr op _ _ _) = opMap ! op $ instr

addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr :: Op
addr (Instr _ rA rB rC) rs = update rC (rs !! rA  +  rs !! rB) rs
mulr (Instr _ rA rB rC) rs = update rC (rs !! rA  *  rs !! rB) rs
banr (Instr _ rA rB rC) rs = update rC (rs !! rA .&. rs !! rB) rs
borr (Instr _ rA rB rC) rs = update rC (rs !! rA .|. rs !! rB) rs
addi (Instr _ rA vB rC) rs = update rC (rs !! rA  +  vB) rs
muli (Instr _ rA vB rC) rs = update rC (rs !! rA  *  vB) rs
bani (Instr _ rA vB rC) rs = update rC (rs !! rA .&. vB) rs
bori (Instr _ rA vB rC) rs = update rC (rs !! rA .|. vB) rs
setr (Instr _ rA  _ rC) rs = update rC (rs !! rA) rs
seti (Instr _ vA  _ rC) rs = update rC vA rs
gtir (Instr _ vA rB rC) rs = update rC (fromEnum $ vA  > rs !! rB) rs
eqir (Instr _ vA rB rC) rs = update rC (fromEnum $ vA == rs !! rB) rs
gtri (Instr _ rA vB rC) rs = update rC (fromEnum $ rs !! rA  > vB) rs
eqri (Instr _ rA vB rC) rs = update rC (fromEnum $ rs !! rA == vB) rs
gtrr (Instr _ rA rB rC) rs = update rC (fromEnum $ rs !! rA  > rs !! rB) rs
eqrr (Instr _ rA rB rC) rs = update rC (fromEnum $ rs !! rA == rs !! rB) rs

ops     = [addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr]
opNames = ["addr","addi","mulr","muli","banr","bani","borr","bori","setr","seti","gtir","gtri","gtrr","eqir","eqri","eqrr"]

opNameToOp :: String -> Op
opNameToOp = (!!!) . M'.fromList $ zip opNames ops

parse :: [String] -> ([Sample], [Instruction])
parse ("":"":instrs) = ([], parseInstrs instrs)
parse (before:instr:after:_:rest) =
    let beforeRegs = fromList . read . drop 8 $ before
        afterRegs  = fromList . read . drop 8 $ after
        (op:a:b:c:[]) = map read . words $ instr
        (samples, instrs) = parse rest
    in  ((Instr op a b c, beforeRegs, afterRegs):samples, instrs)

parseInstrs :: [String] -> [Instruction]
parseInstrs [] = []
parseInstrs (instr:rest) =
    let (op:a:b:c:[]) = map read . words $ instr
    in  (Instr op a b c):(parseInstrs rest)

opCount :: Sample -> Int
opCount (instr, beforeRegs, afterRegs) = length . filter (\op -> afterRegs == op instr beforeRegs) $ ops

opIdentify :: Sample -> OpsMap -> OpsMap
opIdentify (instr@(Instr opCode _ _ _), beforeRegs, afterRegs) m =
    foldr (\(op, opName) m' -> if afterRegs == op instr beforeRegs then insertWith S.union opCode (singleton opName) m' else m') m $ zip ops opNames

reduceOpMap :: OpsMap -> OpMap
reduceOpMap = reduceOpMapRec M.empty
    where reduceOpMapRec assigned m =
            let (singletons, rest) = partition ((== 1) . size) m
                singletonsSet = foldr S.union S.empty singletons
                newAssigned = M.union assigned singletons
            in  if   null rest
                then fmap (opNameToOp . head . toList) newAssigned
                else reduceOpMapRec newAssigned $ fmap (\\ singletonsSet) rest

part1 :: [Sample] -> Int
part1 = length . filter (>= 3) . map opCount

part2a :: [Sample] -> OpMap
part2a samples = reduceOpMap . foldr opIdentify M.empty $ samples

part2b :: OpMap -> [Instruction] -> Registers
part2b opMap = foldr (applyInstruction opMap) initialRegisters . reverse

main :: IO ()
main = do
    (samples, instrs) <- parse . lines <$> readFile "input/16.txt"
    print $ part1 samples
    let opMap = part2a samples
    print $ part2b opMap instrs
