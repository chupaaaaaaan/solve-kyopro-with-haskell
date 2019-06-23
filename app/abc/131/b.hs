main :: IO ()
main = do
  [n,l] <- map read . words <$> getLine :: IO [Int]
  let wa = n * l + (n * (n+1)) `div` 2 - n
      sa = map ((wa - ) . (wa - ) . aji l) [1..n]
  print $ snd . minimum . map (\(x,y) -> (abs x,wa - y)) $ zip sa sa
  

aji :: Int -> Int -> Int
aji l' i = l' + i - 1
