main :: IO ()
main = do
  w <- getLine
  putStrLn $ filter p w

p :: Char -> Bool
p a = a /= 'a' && a /= 'i' && a /= 'u' && a /= 'e' && a /= 'o'
