module PascalTriangle where

import Control.Monad
import Data.Array.ST
import Data.Array.Unboxed

pascal :: Int -> Int -> UArray (Int,Int) Int
pascal n0 k0 = runSTUArray $ do
  dptbl <- newArray ((0,0),(n0,k0)) 0
  forM_ [(n',k') | n' <- [0..n0], k' <- [0..k0], n'>=k'] $ \(n,k) ->
    if k == 0 || k == n
    then writeArray dptbl (n,k) 1
    else liftM2 (+) (readArray dptbl (n-1,k-1)) (readArray dptbl (n-1,k)) >>= writeArray dptbl (n,k)
  return dptbl
