module Memory where

import Zipper

type Acc = Int
type Pos = Int

type Mem = (Acc, Pos, Zipper Int)

emptyMem :: Mem
emptyMem = (0, 1, ([], [0,0..]))

goto :: Pos -> Mem -> Mem
goto dest (acc, pos, zipp)
  | dest < 1 = error "invalid position"
  | pos < dest =
    goto dest (acc, pos + 1, moveRight zipp)
  | pos > dest =
    goto dest (acc, pos - 1, moveLeft zipp)
  | otherwise =
    (acc, pos, zipp)

storeHere :: Mem -> Mem
storeHere (acc, pos, zipp) = (acc, pos, place acc zipp)

storeAt :: Pos -> Mem -> Mem
storeAt pos mem = storeHere $ goto pos mem

loadConst :: Acc -> Mem -> Mem
loadConst x (_, p, z) = (x, p, z)

loadCell :: Mem -> Mem
loadCell (_, p, z) =
  case getItem z of
    Just x -> (x, p, z)
    Nothing -> error "nonexistent cell"

showCell :: Mem -> Int
showCell (_, _, z) =
  case getItem z of
    Just x -> x
    Nothing -> error "nonexistent cell"

getCell :: Pos -> Mem -> Int
getCell p m@(acc, _, _) 
  | p == 0 = acc 
  | otherwise = showCell $ goto p m

putCell :: Pos -> Int -> Mem -> Mem
putCell k v (acc, p, z)
  | k == 0 = (v, p, z)
  | otherwise =
    let (_, p', z') = storeAt k (v, p, z)
    in (acc, p', z') 

getAcc :: Mem -> Int
getAcc (acc, _, _) = acc

snapshot :: Mem -> Mem
snapshot (acc, pos, (l, r)) = (acc, pos, (l, take 10 r))