{-# LANGUAGE ScopedTypeVariables #-}

module My.Data.BinomialHeap( BinomialHeap
                           , fromListMax
                           , fromListMin
                           , module My.Data.Heap
) where

import My.Data.Heap
import My.Data.Comparable
import Data.Proxy

data Tree a b = Node Int b [Tree a b] deriving Show
data BinomialHeap a b = BH [Tree a b]

-- fromListMax :: Ord b => [b] -> LeftistHeap Greater b
-- fromListMax [] = E
-- fromListMax (x:xs) = insert x $ fromListMax xs

-- fromListMin :: Ord b => [b] -> LeftistHeap Lesser b
-- fromListMin [] = E
-- fromListMin (x:xs) = insert x $ fromListMin xs

link :: Tree a b -> Tree a b -> Tree a b
link (t1@(Node r x1 c1)) (t2@(Node _ x2 c2)) =
  if x1 <= x2
  then Node (r + 1) x1 (t2:c1)
  else Node (r + 1) x2 (t1:c2)


rank :: Tree a b -> Int
rank (Node r _ _) = r

root :: Tree a b -> b
root (Node _ x _) = x

insTree :: Tree a b -> [Tree a b]
insTree t [] = [t]
insTree t (ts@(t':ts')) =
  if rank t < rank t'
  then t:ts
  else insTree (link t t') ts'

mrgTree ts [] = ts
mrgTree [] ts = ts
mrgTree (ts1@(t1:ts1')) (ts2@(t2:ts2'))
  = case compare (rank t1) (rank t2) of
      LT -> t1:mrgTree ts1' ts2
      GT -> t2:mrgTree ts1 ts2'
      EQ -> insTree (link t1 t2) (mrgTree ts1' ts2')




instance Comparable a => Heap (BinomialHeap a) where
  empty                   = BH []

  isEmpty (BH ts)         = null ts

  insert x ts             = BH $ insTree (Node 0 x []) ts

  merge (BH ts1) (BH ts2) = BH $ mrgTree ts1 ts2

  find E = Nothing
  find (T _ x _ _) = Just x

  delete E = Nothing
  delete (T _ _ a b) = Just (merge a b)

