main :: IO ()
main = do
  [a,b,c] <- map read . words <$> getLine :: IO [Int]
  putStrLn $ if (a <= c && c <= b) || (b <= c && c <= a) then "Yes" else "No"
