module My.Data.HeapImpl( Heap
                       , fromList
                       ) where

data Heap a = E | T Int a (Heap a) (Heap a) deriving Show

-- Auxiliary Methods
fromList :: Ord a => [a] -> Heap a
fromList = foldr insert E

rank :: Heap a -> Int
rank E = 0
rank (T r _ _ _) = r

makeT :: a -> Heap a -> Heap a -> Heap a
makeT x a b = if rank a >= rank b
              then T (rank b + 1) x a b
              else T (rank a + 1) x b a


-- Heap Methods
empty :: Ord a => Heap a
empty = E

isEmpty :: Ord a => Heap a -> Bool
isEmpty E = True
isEmpty _ = False

insert :: Ord a => a -> Heap a -> Heap a
insert x = merge (T 1 x E E)

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h E = h
merge E h = h
merge h1@(T _ x a1 b1) h2@(T _ y a2 b2)
  = if (<=) x y
    then makeT x a1 $ merge b1 h2
    else makeT y a2 $ merge h1 b2

find :: Ord a => Heap a -> Maybe a
find E = Nothing
find (T _ x _ _) = Just x

delete :: Ord a => Heap a -> Maybe (Heap a)
delete E = Nothing
delete (T _ _ a b) = Just (merge a b)

