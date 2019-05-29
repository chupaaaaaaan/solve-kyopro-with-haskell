main :: IO ()
main = do
  s <- getLine
  k <- readLn :: IO Integer
  putStrLn $ solve s k

solve :: String -> Integer -> String
solve ss y
  | length (takeWhile (=='1') ss) >= fromInteger y = "1"
  | otherwise = [head . dropWhile (=='1') $ ss]
