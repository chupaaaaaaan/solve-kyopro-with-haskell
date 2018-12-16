import Control.Monad
main :: IO ()
main = do
  [n,x] <- map read . words <$> getLine
  ms <- map read <$> replicateM n getLine
  print $ n + (x - sum ms) `div` minimum ms
