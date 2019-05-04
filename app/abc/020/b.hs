main :: IO ()
main = do
  x <- (*2) . read . concat . words <$> getLine :: IO Int
  putStrLn . show $ x
