main :: IO ()
main = do
  s <- getLine
  t <- getLine
  let [q,l,r,u,d] = flip f s <$> "?LRUD"
      f c = length . filter (==c)
      b = abs (l - r) + abs (u - d)

  if t == "1"
    then print $ b + q
    else if b >= q
         then print $ b - q
         else print $ (q - b) `mod` 2

