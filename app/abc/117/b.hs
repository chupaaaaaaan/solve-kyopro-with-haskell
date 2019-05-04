main :: IO ()
main = do
  getLine
  ls <- map read . words <$> getLine
  putStrLn $ if 2 * (maximum ls) < sum ls then "Yes" else "No"
