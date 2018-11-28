import Control.Monad
import Data.Array

main :: IO ()
main = do
  n <- read <$> getLine
  as <- listArray (0,n-1) <$>  replicateM n (read <$> getLine)

  print $ f n as 0 1

f :: Int -> Array Int Int -> Int -> Int -> Int
f n xs m i
  | i == 2 = m
  | n == m = -1
  | otherwise = f n xs (m+1) (xs!(i-1))
