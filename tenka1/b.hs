main :: IO ()
main = do
  _ <- getLine
  s <- getLine
  k <- readLn :: IO Int
  let c = s !! (k-1)
  putStrLn $ map (\x -> if x/=c then '*' else x) s
  
