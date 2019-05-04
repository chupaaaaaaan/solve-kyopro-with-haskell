{-# LANGUAGE ScopedTypeVariables #-}

module My.Data.WeightBiasedLeftistHeap
  ( WeightBiasedLeftistHeap
  , fromListMax
  , fromListMin
  , module My.Data.Heap
) where

import My.Data.Heap
import My.Data.Comparable
import Data.Proxy

data WeightBiasedLeftistHeap a b = E | T Int b (WeightBiasedLeftistHeap a b) (WeightBiasedLeftistHeap a b) deriving Show

fromListMax :: Ord b => [b] -> WeightBiasedLeftistHeap Greater b
fromListMax [] = E
fromListMax (x:xs) = insert x $ fromListMax xs

fromListMin :: Ord b => [b] -> WeightBiasedLeftistHeap Lesser b
fromListMin [] = E
fromListMin (x:xs) = insert x $ fromListMin xs

size :: WeightBiasedLeftistHeap a b -> Int
size E = 0
size (T r _ _ _) = r

makeT :: b -> WeightBiasedLeftistHeap a b -> WeightBiasedLeftistHeap a b -> WeightBiasedLeftistHeap a b
makeT x a b = if size a >= size b
              then T (size a + size b + 1) x a b
              else T (size a + size b + 1) x b a

instance Comparable a => Heap (WeightBiasedLeftistHeap a) where
  empty     = E

  isEmpty E = True
  isEmpty _ = False

  insert x h = merge (T 1 x E E) h

  merge h E = h
  merge E h = h
  merge (h1@(T _ x a1 b1)) (h2@(T _ y a2 b2))
    = if comp (Proxy :: Proxy a) x y
      then makeT x a1 $ merge b1 h2
      else makeT y a2 $ merge h1 b2

  find E = Nothing
  find (T _ x _ _) = Just x

  delete E = Nothing
  delete (T _ _ a b) = Just (merge a b)

