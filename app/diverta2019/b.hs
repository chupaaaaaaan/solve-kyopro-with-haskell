main :: IO ()
main = do
  [r,g,b,n] <- map read . words <$> getLine
  print $ solve r g b n

solve :: Int -> Int -> Int -> Int -> Int
solve r g b n = go 0 0 0
  where go x 0 a
          | n - r * x < 0 = a
          | n - r * x >= 0 && (n - r * x) `mod` b == 0 = go x 1 (a + 1)
          | otherwise = go x 1 a
        go x y a
          | n - r * x - g * y < 0 = go (x + 1) 0 a
          | n - r * x - g * y >= 0 && (n - r * x - g * y) `mod` b == 0 = go x (y + 1) (a + 1)
          | otherwise = go x (y + 1) a
