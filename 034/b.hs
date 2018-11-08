main :: IO ()
main = do
  n <- read <$> getLine :: IO Integer
  if even n
    then print $ n - 1
    else print $ n + 1
