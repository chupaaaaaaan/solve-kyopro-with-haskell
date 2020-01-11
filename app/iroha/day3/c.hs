main :: IO ()
main = do
  [n,k] <- map read . words <$> getLine
  
