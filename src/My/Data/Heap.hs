module My.Data.Heap( Heap(..)
                   ) where

class Heap h where
  empty     :: Ord a => h a
  isEmpty   :: Ord a => h a -> Bool

  insert    :: Ord a => a -> h a -> h a
  merge     :: Ord a => h a -> h a -> h a

  find   :: Ord a => h a -> Maybe a
  delete :: Ord a => h a -> Maybe (h a)

