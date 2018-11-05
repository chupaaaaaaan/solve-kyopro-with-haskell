main :: IO ()
main = do
  [n,t] <- ((map read . words) <$> getLine) :: IO [Int]
  [c0,t0] <- (foldr (g t) [1001,1001]) <$> (sequence $ replicate n ((map read . words) <$> getLine :: IO [Int]))
  if t0 <= t
    then putStrLn $ show c0
    else putStrLn "TLE"
  where
    g :: Int -> [Int] -> [Int] -> [Int]
    g t [a0,b0] [a1,b1] = if a0 <= a1 && b0 <= t
                        then [a0,b0]
                        else [a1,b1]
