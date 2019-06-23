import Control.Monad
import Data.Array.ST
import Data.Array.Unboxed
import Data.STRef

dex :: Int
dex = 1000000007

main :: IO ()
main = do
  [n,m] <- map read . words <$> getLine
  if m==0
    then print $ solve' n ! n
    else do as <- replicateM m readLn :: IO [Int]
            print $ solve (listArray (1,m) as) n m ! n

solve' :: Int -> UArray Int Int
solve' n = runSTUArray $ do
  dptbl <- newArray (1,n) 0
  forM_ [1..n] $ \i ->
    case i of
      1 -> writeArray dptbl 1 1
      2 -> liftM2 (+) (readArray dptbl 1) (return 1) >>= writeArray dptbl 2
      _ -> liftM2 (\x y -> (x+y)`mod`dex) (readArray dptbl (i-1)) (readArray dptbl (i-2)) >>= writeArray dptbl i
  return dptbl

solve :: UArray Int Int -> Int -> Int -> UArray Int Int
solve xs n m = runSTUArray $ do
  dptbl <- newArray (1,n) 0
  ref   <- newSTRef 1
  forM_ [1..n] $ \i -> do
    p <- readSTRef ref
    if i == (xs!p)
      then writeSTRef ref (if p==m then p else p+1)
      else case i of
             1 -> writeArray dptbl 1 1
             2 -> liftM2 (+) (readArray dptbl 1) (return 1) >>= writeArray dptbl 2
             _ -> liftM2 (\x y -> (x+y)`mod`dex) (readArray dptbl (i-1)) (readArray dptbl (i-2)) >>= writeArray dptbl i

  return dptbl
