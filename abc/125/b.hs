main :: IO ()
main = do
  _ <- getLine
  vs <- map read . words <$> getLine :: IO [Int]
  cs <- map read . words <$> getLine :: IO [Int]
  print $ sum $ zipWith (\x y -> if x > y then x - y else 0) vs cs
