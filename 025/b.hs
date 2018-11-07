import Control.Monad

main :: IO ()
main = do
  [n,a,b] <- map read . words <$> getLine :: IO [Int]
  s <- sum <$> replicateM n (change a b . words <$> getLine)
  if s == 0
    then print s
    else if s > 0
         then putStrLn $ "East " ++ show s
         else putStrLn $ "West " ++ show (negate s)

change :: Int -> Int -> [String] -> Int
change a b [x,y]
  | x == "East" = f a b $ read y
  | otherwise = negate $ f a b $ read y
  
f :: Int -> Int -> Int -> Int
f a b x
  | x < a = a
  | x > b = b
  | otherwise = x
