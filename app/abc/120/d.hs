import Control.Monad

main :: IO ()
main = do
  [n,m] <- map read . words <$> getLine
  abss <- replicateM m $ (\[x,y] -> (x,y)) .map read . words <$> getLine
  
  
