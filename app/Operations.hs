module Operations where

import Memory

data Arg =
    Const Int
  | Cell Int
  | Ptr Int
  deriving Show

data Op =
    Load Arg
  | Store Arg
  | Add Arg
  | Sub Arg
  | Mul Arg
  | Div Arg
  deriving Show

deref :: Arg -> Mem -> Arg
deref (Ptr p) m = Cell (getCell p m)
deref a _ = a

exec :: Op -> Mem -> Mem
exec (Store (a@(Ptr _))) m = exec (Store (deref a m)) m
exec (Load (a@(Ptr _))) m = exec (Load (deref a m)) m
exec (Add (a@(Ptr _))) m = exec (Add (deref a m)) m
exec (Sub (a@(Ptr _))) m = exec (Sub (deref a m)) m
exec (Mul (a@(Ptr _))) m = exec (Mul (deref a m)) m
exec (Div (a@(Ptr _))) m = exec (Div (deref a m)) m

exec (Store (Cell k)) m = storeAt k m
exec (Load (Cell k)) m = loadCell $ goto k m
exec (Add (Cell k)) m@(acc, p, z) = (acc + x, p, z)
  where x = getCell k m
exec (Sub (Cell k)) m@(acc, p, z) = (acc - x, p, z)
  where x = getCell k m
exec (Mul (Cell k)) m@(acc, p, z) = (acc * x, p, z)
  where x = getCell k m
exec (Div (Cell k)) m@(acc, p, z) = (acc `div` x, p, z)
  where x = getCell k m

exec (Store (Const _)) _ = error "STORE argument must be a cell or pointer"
exec (Load (Const x)) m = loadConst x m
exec (Add (Const x)) (acc, p, z) = (acc + x, p, z)
exec (Sub (Const x)) (acc, p, z) = (acc - x, p, z)
exec (Mul (Const x)) (acc, p, z) = (acc * x, p, z)
exec (Div (Const x)) (acc, p, z) = (acc `div` x, p, z)

readTo :: Int -> Arg -> Mem -> Mem
readTo v (Cell k) m  = putCell k v m
readTo v p@(Ptr _) m = readTo v (deref p m) m
readTo _ (Const _) _ = error "READ argument must be a cell or pointer"

writeFrom :: Arg -> Mem -> Int
writeFrom (Cell k) m = getCell k m
writeFrom p@(Ptr _) m = writeFrom (deref p m) m
writeFrom (Const x) _ = x