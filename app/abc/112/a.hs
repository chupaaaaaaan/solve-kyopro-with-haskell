main :: IO ()
main = do
  x <- getLine
  case x of
    "1" -> putStrLn "Hello World"
    "2" -> do
      a <- (read <$> getLine) :: IO Int
      b <- (read <$> getLine) :: IO Int
      putStrLn $ show (a + b)
      
