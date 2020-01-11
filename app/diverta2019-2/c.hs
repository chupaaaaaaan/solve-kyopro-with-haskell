{-# LANGUAGE ScopedTypeVariables #-}
import Data.Maybe
import Data.Proxy
import Control.Monad.State

data LeftistHeap a b = E | T Int b (LeftistHeap a b) (LeftistHeap a b) deriving Show

fromListMax :: Ord b => [b] -> LeftistHeap Greater b
fromListMax = foldr insert E

fromListMin :: Ord b => [b] -> LeftistHeap Lesser b
fromListMin = foldr insert E

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

class Heap h where
  empty     :: Ord a => h a
  isEmpty   :: Ord a => h a -> Bool

  insert    :: Ord a => a -> h a -> h a
  merge     :: Ord a => h a -> h a -> h a

  find   :: Ord a => h a -> Maybe a
  delete :: Ord a => h a -> Maybe (h a)


data Lesser
data Greater

class Comparable tag where
  comp :: Ord a => Proxy tag -> a -> a -> Bool

instance Comparable Lesser where
  comp _ = (<=)

instance Comparable Greater where
  comp _ = (>=)

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- fromListMin . map read . words <$> getLine :: IO (LeftistHeap Lesser Int)
  let m1 = fromJust $ find as
      h1 = fromJust $ delete as
      m2 = fromJust $ find h1
      h2 = fromJust $ delete h1

  print $ solve h2 m1 m2
  solve' h2 m1 m2
  
solve :: LeftistHeap Lesser Int -> Int -> Int -> Int
solve bs a b
  | isEmpty bs = b - a
  | otherwise =
      solve (fromJust $ delete bs) (a - b) (fromJust $ find bs)

solve' :: LeftistHeap Lesser Int -> Int -> Int -> IO ()
solve' bs a b
  | isEmpty bs = putStrLn $ (show b) ++ " " ++ (show a)
  | otherwise = do
      putStrLn $ (show a) ++ " " ++ (show b)
      solve' (fromJust $ delete bs) (a - b) (fromJust $ find bs)
