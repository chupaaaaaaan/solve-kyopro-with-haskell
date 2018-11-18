main :: IO ()
main = do
  [n,k] <- map read . words <$> getLine :: IO [Integer]
  print $ k * (k - 1)^(n - 1)
