main :: IO ()
main = do
  [n,k] <- map read . words <$> getLine
  putStrLn $ if 2 * k - 1 <= n
             then "YES"
             else "NO"
