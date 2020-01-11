class Heap h where
  empty     :: Ord a => h a
  isEmpty   :: Ord a => h a -> Bool

  insert    :: Ord a => a -> h a -> h a
  merge     :: Ord a => h a -> h a -> h a

  findMin   :: Ord a => h a -> Maybe a
  deleteMin :: Ord a => h a -> Maybe (h a)

data LeftistHeap a = E | T Int a (LeftistHeap a) (LeftistHeap a) deriving Show

fromList :: Ord a => [a] -> LeftistHeap a
fromList [] = E
fromList (x:xs) = merge (T 1 x E E) $ fromList xs

rank :: LeftistHeap a -> Int
rank E = 0
rank (T r _ _ _) = r

makeT :: a -> LeftistHeap a -> LeftistHeap a -> LeftistHeap a
makeT x a b = if rank a >= rank b
              then T (rank b + 1) x a b
              else T (rank a + 1) x b a

instance Heap LeftistHeap where
  empty     = E

  isEmpty E = True
  isEmpty _ = False

  merge h E = h
  merge E h = h
  merge (h1@(T _ x a1 b1)) (h2@(T _ y a2 b2))
    = if x <= y
      then makeT x a1 $ merge b1 h2
      else makeT y a2 $ merge h1 b2

  insert x h = merge (T 1 x E E) h

  findMin E = Nothing
  findMin (T _ x _ _) = Just x

  deleteMin E = Nothing
  deleteMin (T _ _ a b) = Just (merge a b)



main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- map read . words <$> getLine :: IO [Int]
  let as1 = take n as
      as2 = take n $ drop n as
      as3 = map negate $ drop (2*n) as
      su1 = sum as1
      su3 = sum as3
      lh1 = fromList as1
      lh3 = fromList as3












-- solvel :: [Int] -> Int -> LeftistHeap Int -> [Int]
solvel xs sm h = (\s -> runState s h) $ do
  
  
    
