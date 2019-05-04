main :: IO ()
main = do
  [n,q] <- map read . words <$> getLine
  
