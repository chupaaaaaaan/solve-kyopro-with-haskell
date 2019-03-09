main :: IO ()
main = do
  s <- getLine
  let o = length . filter (=='1') $ s
      z = length . filter (=='0') $ s
  print $ o + z - abs (o - z)
