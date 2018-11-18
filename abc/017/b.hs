main :: IO ()
main = do
  x <- getLine
  putStrLn . judge $ x

judge :: String -> String
judge a
  | a == "" = "YES"
  | elem (last a) "oku" = judge . init $ a
  | drop (length a - 2) a == "ch" = judge . take (length a - 2) $ a
  | otherwise = "NO"
