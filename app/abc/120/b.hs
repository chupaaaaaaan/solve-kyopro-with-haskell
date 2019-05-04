main :: IO ()
main = do
  [a,b,k] <- map read . words <$> getLine
  let g = gcd a b
  print $ (!!(k-1)) . reverse . filter ((==0) . mod g) $ [1..g]
