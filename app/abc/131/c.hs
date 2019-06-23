main :: IO ()
main = do
  [a,b,c,d] <- map read . words <$> getLine :: IO [Integer]

  let k = (c * d) `div` gcd c d
      l = a `mod` c
      m = a `mod` d
      n = a `mod` k
      l1 = a `div` c
      l2 = b `div` c
      m1 = a `div` d
      m2 = b `div` d
      n1 = a `div` k
      n2 = b `div` k
      l' = l2 - l1 + if l == 0 then 1 else 0
      m' = m2 - m1 + if m == 0 then 1 else 0
      n' = n2 - n1 + if n == 0 then 1 else 0
  print $ (b - a + 1) - (l' + m' - n')
