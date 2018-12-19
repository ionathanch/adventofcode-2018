module Day19 (main) where

import Prelude hiding (length)
import Data.List (elemIndex)
import Data.Maybe (catMaybes)
import Data.Bits ((.|.), (.&.))
import Data.Sequence (Seq, fromList, update, adjust', index, length)

data Op = Op OpType Int Int Int
data OpType =
    Addr | Addi | Mulr | Muli | Banr | Bani | Borr | Bori |
    Setr | Seti | Gtir | Gtri | Gtrr | Eqir | Eqri | Eqrr
    deriving Enum
type Ops = Seq Op
type Registers = Seq Int

infixl 9 !
(!) = index

execOp :: Op -> Registers -> Registers
execOp (Op Addr rA rB rC) rs = update rC (rs ! rA  +  rs ! rB) rs
execOp (Op Mulr rA rB rC) rs = update rC (rs ! rA  *  rs ! rB) rs
execOp (Op Banr rA rB rC) rs = update rC (rs ! rA .&. rs ! rB) rs
execOp (Op Borr rA rB rC) rs = update rC (rs ! rA .|. rs ! rB) rs
execOp (Op Addi rA vB rC) rs = update rC (rs ! rA  +  vB) rs
execOp (Op Muli rA vB rC) rs = update rC (rs ! rA  *  vB) rs
execOp (Op Bani rA vB rC) rs = update rC (rs ! rA .&. vB) rs
execOp (Op Bori rA vB rC) rs = update rC (rs ! rA .|. vB) rs
execOp (Op Setr rA  _ rC) rs = update rC (rs ! rA) rs
execOp (Op Seti vA  _ rC) rs = update rC vA rs
execOp (Op Gtir vA rB rC) rs = update rC (fromEnum $ vA  > rs ! rB) rs
execOp (Op Eqir vA rB rC) rs = update rC (fromEnum $ vA == rs ! rB) rs
execOp (Op Gtri rA vB rC) rs = update rC (fromEnum $ rs ! rA  > vB) rs
execOp (Op Eqri rA vB rC) rs = update rC (fromEnum $ rs ! rA == vB) rs
execOp (Op Gtrr rA rB rC) rs = update rC (fromEnum $ rs ! rA  > rs ! rB) rs
execOp (Op Eqrr rA rB rC) rs = update rC (fromEnum $ rs ! rA == rs ! rB) rs

opNames = ["addr","addi","mulr","muli","banr","bani","borr","bori","setr","seti","gtir","gtri","gtrr","eqir","eqri","eqrr"]

parse :: String -> (Int, Ops)
parse input =
    let ipBinding:rest = lines input
        ip  = read $ drop 4 ipBinding
        ops = fromList $ map parseOp rest
    in  (ip, ops)
    where
        parseOp str =
            let opName:a:b:c:[] = words str
                Just opType = toEnum <$> elemIndex opName opNames
            in  Op opType (read a) (read b) (read c)

-- exec :: the register to which the instruction pointer is bound -> instructions -> initial registers -> final registers
exec :: Int -> Ops -> Registers -> Registers
exec ip ops regs =
    let i = regs ! ip
        nextRegs = adjust' (+1) ip $ execOp (ops ! i) regs
    in  if i >= length ops then regs else exec ip ops nextRegs

part1 :: Int -> Ops -> Int
part1 ip ops = exec ip ops (fromList [0, 0, 0, 0, 0, 0]) ! 0

main :: IO ()
main = do
    (ip, ops) <- parse <$> readFile "input/19.txt"
    print $ part1 ip ops
