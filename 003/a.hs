main :: IO ()
main = do
  n <- (read <$> getLine) :: IO Int
  putStrLn $ show $ (n+1) * 5000
