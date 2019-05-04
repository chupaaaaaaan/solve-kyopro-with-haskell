main :: IO ()
main = do
  [a,b] <- map read . words <$> getLine
  s <- getLine
  let t = takeWhile (/='-') s
      u = takeWhile (/='-') . tail . dropWhile (/='-') $ s

  putStrLn $ if length t == a && length u == b then "Yes" else "No"
