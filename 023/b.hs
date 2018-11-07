main :: IO ()
main = do
  n <- read <$> getLine :: IO Int
  s <- getLine

  if check s && odd n
    then putStrLn . show $ n `div` 2
    else putStrLn "-1"

check :: String -> Bool
check ss = (last ss) == k && (and . map (\(x,y) -> x == y) $ zip ss ks)
  where ks = case (head ss) of
              'a' -> cycle "abc"
              'b' -> cycle "bca"
              'c' -> cycle "cab"
              _   -> "@"
        k = case (head ks) of
              'a' -> 'c'
              'b' -> 'b'
              'c' -> 'a'
              _ -> '@'
