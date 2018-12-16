main :: IO ()
main = do
  a <- readLn :: IO Int
  b <- readLn
  c <- readLn
  d <- readLn
  print $ (min a b) + (min c d)



