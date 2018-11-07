main :: IO ()
main = do
  n <- read <$> getLine :: IO Int
  as <- map read . words <$> getLine :: IO [Int]
  
  if (sum as) `mod` n /= 0
    then putStrLn "-1"
    else putStrLn . show $ solve n as

solve :: Int -> [Int] -> Int
solve n (a:as) = go as 1 a 0
  where p = (sum (a:as)) `div` n
        go [] _ _ b = b
        go (x:xs) i s b
          | s `div` i == p && s `mod` i == 0 = go xs 1 x b
          | otherwise = go xs (i+1) (s+x) (b+1)
