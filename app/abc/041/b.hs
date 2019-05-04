main :: IO ()
main = do
  [a,b,c] <- map read . words <$> getLine :: IO [Integer]
  print $ (a * b * c) `mod` (10^9 + 7)
