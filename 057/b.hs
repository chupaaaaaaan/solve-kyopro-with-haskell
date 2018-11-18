import Control.Monad
main :: IO ()
main = do
  [n,m] <- map read . words <$> getLine
  ab <- replicateM n (map read . words <$> getLine)
  cd <- zip [1..m] <$> replicateM m (map read . words <$> getLine)
  forM_ ab $ \x -> do
    print . fst . foldr1 (f x) $ cd

f :: [Int] -> (Int,[Int]) -> (Int,[Int]) -> (Int,[Int])
f [a,b] (n1,[c1,d1]) (n2,[c2,d2]) = if abs(a-c1)+abs(b-d1) <= abs(a-c2)+abs(b-d2) then (n1,[c1,d1]) else (n2,[c2,d2])
