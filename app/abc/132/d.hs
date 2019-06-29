import Control.Monad
import Data.Array.ST
import Data.Array.Unboxed

dex :: Int
dex = 1000000007

main :: IO ()
main = do
  [n,k] <- map read . words <$> getLine
  solve n k $ pascal n k
  
solve :: Int -> Int -> UArray (Int,Int) Int -> IO ()
solve n0 k0 b1 = forM_ [1..k0] $ \i -> print $ ((b1!(n0-k0+1,i)) * (b1!(k0-1,i-1))) `mod` dex

pascal :: Int -> Int -> UArray (Int,Int) Int
pascal n0 k0 = runSTUArray $ do
  dptbl <- newArray ((0,0),(n0,k0)) 0
  forM_ [(n',k') | n' <- [0..n0], k' <- [0..k0], n'>=k'] $ \(n,k) ->
    if k == 0 || k == n
    then writeArray dptbl (n,k) 1
    else liftM2 (\x y -> (x+y)`mod`dex) (readArray dptbl (n-1,k-1)) (readArray dptbl (n-1,k)) >>= writeArray dptbl (n,k)
  return dptbl
