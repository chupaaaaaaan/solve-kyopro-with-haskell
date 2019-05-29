{-# LANGUAGE ScopedTypeVariables #-}

module My.Data.LeftistHeap( LeftistHeap
                          , fromListMax
                          , fromListMin
                          , module My.Data.Heap
                          ) where

import My.Data.Heap
import My.Data.Comparable
import Data.Proxy

data LeftistHeap a b = E | T Int b (LeftistHeap a b) (LeftistHeap a b) deriving Show

fromListMax :: Ord b => [b] -> LeftistHeap Greater b
fromListMax = foldr insert E

fromListMin :: Ord b => [b] -> LeftistHeap Lesser b
fromListMin = foldr insert E


-- fromListMin [3,2,1,6,5,4,10,9]
-- 1 - 4 - 9 - 10 - E
--   |   |   |    |
--   |   |   |    - E
--   |   |   |
--   |   |   - E
--   |   |
--   |   - 5 - 6 - E
--   |       |   |
--   |       |   - E
--   |       |
--   |       - E
--   |
--   - 2 - 3 - E
--       |   |
--       |   - E
--       |
--       - E

rank :: LeftistHeap a b -> Int
rank E = 0
rank (T r _ _ _) = r

makeT :: b -> LeftistHeap a b -> LeftistHeap a b -> LeftistHeap a b
makeT x a b = if rank a >= rank b
              then T (rank b + 1) x a b
              else T (rank a + 1) x b a

instance Comparable a => Heap (LeftistHeap a) where
  empty     = E

  isEmpty E = True
  isEmpty _ = False

  insert x = merge (T 1 x E E)

  merge h E = h
  merge E h = h
  merge h1@(T _ x a1 b1) h2@(T _ y a2 b2)
    = if comp (Proxy :: Proxy a) x y
      then makeT x a1 $ merge b1 h2
      else makeT y a2 $ merge h1 b2

  find E = Nothing
  find (T _ x _ _) = Just x

  delete E = Nothing
  delete (T _ _ a b) = Just (merge a b)

