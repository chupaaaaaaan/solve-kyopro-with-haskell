import Numeric
main :: IO ()
main = do
  [t,x] <- map read . words <$> getLine :: IO [Double]
  putStrLn $ showFFloat (Just 10) (t / x) ""
