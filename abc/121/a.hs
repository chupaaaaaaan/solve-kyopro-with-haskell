main :: IO ()
main = do
  [h1,w1] <- map read . words <$> getLine
  [h2,w2] <- map read . words <$> getLine
  print $ (h1-h2) * (w1-w2)
  
