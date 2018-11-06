main :: IO ()
main = do
  _ <- (read <$> getLine) :: IO Int
  as <- (filter (/=0) . map read . words <$> getLine) :: IO [Int]

  putStrLn . show $ sum as `f` length as

f :: Int -> Int -> Int
f a b
  | a `mod` b == 0 = a `div` b
  | otherwise = a `div` b + 1
