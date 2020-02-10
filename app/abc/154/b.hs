main :: IO ()
main = do
  s <- getLine
  putStrLn $ map (\x -> 'x') s
