module Main (main) where

import Memory (Mem, emptyMem, getAcc)
import Program 
import Operations (exec, readTo, writeFrom)
import Parser (parseProgram)
import System.Environment (getArgs)

type Machine = (Prog, [Int], [Int], Mem, Bool)

step :: Machine -> Machine
step m@(p, inp, out, mem, halt)
  | halt = m 
  | otherwise = case current p of
    Nothing -> (p, inp, out, mem, True)
    Just i -> case i of
      MemOp op -> (stepProg p, inp, out, exec op mem, halt)
      Read dest -> case inp of
        [] -> error "Empty input"
        x:xs -> (stepProg p, xs, out, readTo x dest mem, halt)
      Write src -> (stepProg p, inp, (writeFrom src mem) : out, mem, halt)
      Jump pos -> (goto pos p, inp, out, mem, halt)
      Jzero pos -> 
        if getAcc mem == 0 then (goto pos p, inp, out, mem, halt)
        else (stepProg p, inp, out, mem, halt)
      Jgtz pos ->
        if getAcc mem > 0 then (goto pos p, inp, out, mem, halt)
        else (stepProg p, inp, out, mem, halt)
      Halt -> (stepProg p, inp, out, mem, True)

go :: Machine -> [Int]
go m@(_, _, out, _, halt)
  | halt = reverse out
  | otherwise = go $ step m

run :: [Instr] -> [Int] -> [Int]
run ins inp = go (makeProg ins, inp, [], emptyMem, False)

main :: IO ()
main = do
  (file : rest) <- getArgs
  src <- readFile file
  let inp = map read rest :: [Int]
  print $ run (parseProgram src) inp