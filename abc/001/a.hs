main :: IO ()
main = do
  h1 <- (read <$> getLine) :: IO Int
  h2 <- (read <$> getLine) :: IO Int
  putStrLn $ show $ h1 - h2
