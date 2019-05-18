main :: IO ()
main = do
  [n,k] <- map read . words <$> getLine
  print $ n - (k - 1)
