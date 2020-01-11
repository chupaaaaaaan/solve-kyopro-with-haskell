import My.Data.WeightBiasedLeftistHeap
import Control.Monad.State
import Data.Maybe


main :: IO ()
main = do
  let l2l = fromListMin [3,2,1,6,5,4,10,9]
      l2r = [20,2,38,22,56] :: [Int]
  print l2l

  print $ solve l2r l2l

solve :: Heap h => [Int] -> h Int -> [Int]
solve xs h = (`evalState`(h,0)) $
  forM xs $ \x -> do
    (ho,n) <- get
    let ht = insert x ho
        m = fromJust . find $ ht
        hn = fromJust . delete $ ht
        n' = n + x - m
    put (hn,n')
    return n'
  
