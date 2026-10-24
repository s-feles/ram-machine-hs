module Parser where

import Operations ( Op(..), Arg(..) )
import Data.Char ( toUpper, isDigit )
import Program ( Instr(..) )

data Token = TLabel String | TWordArg String | TConst Int | TCell Int | TPtr Int | TNewline

data PInstr = PMem Op | PRead Arg | PWrite Arg | PJump String | PJzero String | PJgtz String | PHalt

type Line = (Int, Maybe String, PInstr)

toInt :: String -> Int
toInt s = read s :: Int

tokenize :: String -> [Token]
tokenize s = map tok $ words $ takeWhile (/= ';') s
  where 
    tok w
      | last w == ':' = TLabel (init w)
      | head w == '=' = TConst $ toInt $ tail w
      | head w == '^' = TPtr $ toInt $ tail w
      | all isDigit w = TCell $ toInt w
      | otherwise = TWordArg w

parseBody :: [Token] -> PInstr
parseBody [] = error "Nothing to parse"
parseBody [TWordArg op]
  | map toUpper op == "HALT" = PHalt
  | otherwise = error "Only Halt is zero-ary"
parseBody [TWordArg op, TWordArg target]
  | map toUpper op == "JUMP" = PJump target
  | map toUpper op == "JZERO" = PJzero target
  | map toUpper op == "JGTZ" = PJgtz target
  | otherwise = error "Argument of jump must be label"
parseBody [TWordArg op, a] = case map toUpper op of
  "LOAD" -> PMem $ Load $ toArg a
  "STORE" -> PMem $ Store $ toArg a
  "ADD" -> PMem $ Add $ toArg a
  "SUB" -> PMem $ Sub $ toArg a
  "MULT" -> PMem $ Mul $ toArg a
  "DIV" -> PMem $ Div $ toArg a
  "READ" -> PRead $ toArg a
  "WRITE" -> PWrite $ toArg a
  _ -> error "Unknown instruction"
  where
    toArg (TConst x) = Const x
    toArg (TCell x) = Cell x
    toArg (TPtr x) = Ptr x
    toArg _ = error "Invalid argument"
parseBody _ = error "Line body must start with an operator"

parse :: Int -> [Token] -> Line
parse n (TLabel l : toks) = (n, Just l, parseBody toks)
parse n toks = (n, Nothing, parseBody toks)

collect :: [Line] -> [(String, Int)]
collect ls = foldl' aux [] ls
  where 
    aux :: [(String, Int)] -> Line -> [(String, Int)]
    aux pairs (n, Just l, _) = (l, n) : pairs
    aux pairs (_, Nothing, _) = pairs

resolve :: [Line] -> [(String, Int)] -> [Instr]
resolve lins labels = map aux lins
  where
    aux :: Line -> Instr
    aux (_, _, PMem op) = MemOp op
    aux (_, _, PRead a) = Read a
    aux (_, _, PWrite a) = Write a
    aux (_, _, PHalt) = Halt
    aux (_, _, PJump l) = case lookup l labels of
      Just n -> Jump n
      Nothing -> error "Cannot resolve label"
    aux (_, _, PJzero l) = case lookup l labels of
      Just n -> Jzero n
      Nothing -> error "Cannot resolve label"
    aux (_, _, PJgtz l) = case lookup l labels of
      Just n -> Jgtz n
      Nothing -> error "Cannot resolve label"

parseProgram :: String -> [Instr]
parseProgram src = resolve lins (collect lins)
  where
    numbered = zip [1..] $ filter (/= "") $ lines src
    lins = [ parse n toks | (n, s) <- numbered, let toks = tokenize s ]