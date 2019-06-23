import Control.Monad

main :: IO ()
main = do
  [p,q,r] <- map read . words <$> getLine
  print $ minimum [p+q, q+r, r+p]
  
