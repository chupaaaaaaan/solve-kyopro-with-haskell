main :: IO ()
main = do
  a <- readLn :: IO Int
  b <- readLn :: IO Int
  c <- readLn :: IO Int
  d <- readLn :: IO Int
  e <- readLn :: IO Int
  k <- readLn :: IO Int
  
  putStrLn $ if and [(b-a)<=k
                    ,(c-a)<=k
                    ,(d-a)<=k
                    ,(e-a)<=k
                    ,(c-b)<=k
                    ,(d-b)<=k
                    ,(e-b)<=k
                    ,(d-c)<=k
                    ,(e-c)<=k
                    ,(e-d)<=k]
             then "Yay!"
             else ":("
