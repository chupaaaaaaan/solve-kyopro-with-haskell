main :: IO ()
main = do
  [n,x] <- map read . words <$> getLine :: IO [Int]
  print $ min (x - 1) (n - x)
