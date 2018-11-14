main :: IO ()
main = do
  [a,b,x] <- map read . words <$> getLine :: IO [Integer]
  let f = a `div` x
      c = a - f * x
      d = b - f * x
      e = if c == 0 then d `div` x + 1 else d `div` x
  print e
