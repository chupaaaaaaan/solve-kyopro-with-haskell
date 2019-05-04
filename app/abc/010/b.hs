main :: IO ()
main = do
  _ <- (read <$> getLine) :: IO Int
  as <- (map read . words <$> getLine) :: IO [Int]
  
  putStrLn . show . sum . map f $ as

f :: Int -> Int
f n
  | n `mod` 6 == 0 = 3
  | n `mod` 6 == 1 = 0
  | n `mod` 6 == 2 = 1
  | n `mod` 6 == 3 = 0
  | n `mod` 6 == 4 = 1
  | n `mod` 6 == 5 = 2
