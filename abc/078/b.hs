main :: IO ()
main = do
  [x,y,z] <- map read . words <$> getLine
  print $ if x `mod` (y+z) < z then x `div` (y+z) - 1 else x `div` (y+z)
