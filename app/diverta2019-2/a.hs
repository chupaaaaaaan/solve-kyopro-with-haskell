import Control.Monad

main :: IO ()
main = do
  [n,k] <- map read . words <$> getLine

  print $ if k == 1 then 0 else n - k
  
