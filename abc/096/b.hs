main :: IO ()
main = do
  abc <- map read . words <$> getLine
  k <- readLn
  print $ sum abc + (maximum abc)*(2^k-1)
