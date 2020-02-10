main :: IO ()
main = do
  [s,t] <- words <$> getLine
  [a,b] <- map read . words <$> getLine :: IO [Int]
  u <- getLine

  putStrLn $ if u == s then show (a - 1) ++ " " ++ show b else show a ++ " " ++ show (b - 1)
