main :: IO ()
main = do
  [_,x] <- (map read . words <$> getLine) :: IO [Int]
  as <- (map read . words <$> getLine) :: IO [Int]
  putStrLn . show . sum . map fst . filter (\(_,a) -> a == '1') $ zip as (ri2b x)

ri2b :: Int -> String
ri2b 0 = ""
ri2b n = show (n `mod` 2) ++ ri2b (n `div` 2)
