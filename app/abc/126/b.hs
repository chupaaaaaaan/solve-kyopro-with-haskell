main :: IO ()
main = do
  [s1,s2,t1,t2] <- map (read . (:[])) <$> getLine :: IO [Int]
  putStrLn $ solve (s1*10+s2) (t1*10+t2)

solve :: Int -> Int -> String
solve x y
  | (x == 0 || x >= 13) && (y >= 1 && y <= 12) = "YYMM"
  | (y == 0 || y >= 13) && (x >= 1 && x <= 12) = "MMYY"
  | (x >= 1 && x <= 12) && (y >= 1 && y <= 12) = "AMBIGUOUS"
  | otherwise = "NA"
  


