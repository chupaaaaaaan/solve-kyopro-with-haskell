main :: IO ()
main = do
  n <- read <$> getLine
  print $ f n

f :: Int -> Int
f n
  | n >= 64 = 64
  | n >= 32 = 32
  | n >= 16 = 16
  | n >= 8 = 8
  | n >= 4 = 4
  | n >= 2 = 2
  | otherwise = 1
  
