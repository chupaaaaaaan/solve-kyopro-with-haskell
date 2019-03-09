main :: IO ()
main = do
  [a,b,c] <- map read . words <$> getLine
  let d = b `div` a
  print $ if d < c then d else c
