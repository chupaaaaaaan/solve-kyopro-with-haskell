main :: IO ()
main = do
  [a,p] <- map read . words <$> getLine
  print $ (3 * a + p) `div` 2
