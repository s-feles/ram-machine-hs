module Program where

import Zipper
import Operations(Op, Arg)

data Instr = 
    MemOp Op
  | Read Arg
  | Write Arg
  | Jump Int
  | Jzero Int
  | Jgtz Int
  | Halt
  deriving Show

type Prog = (Int, Zipper Instr)

makeProg :: [Instr] -> Prog
makeProg is = (1, ([], is))

current :: Prog -> Maybe Instr
current (_, p) = getItem p

goto :: Int -> Prog -> Prog
goto pos (pc, p)
  | pos < 0 = error "Invalid program position"
  | pos < pc =
    goto pos (pc - 1, moveLeft p)
  | pos > pc =
    goto pos (pc + 1, moveRight p)
  | otherwise = (pc, p)

stepProg :: Prog -> Prog
stepProg (pc, p) = (pc + 1, moveRight p)