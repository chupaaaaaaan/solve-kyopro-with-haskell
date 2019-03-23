main :: IO ()
main = do
  [n,y] <- map read . words <$> getLine :: IO [Int]
  putStrLn $ case bill n y (0,0) of
    Nothing -> "-1 -1 -1"
    Just (p,q) -> show p ++ " " ++ show q ++ " " ++ show (n - p - q)

bill :: Integral a => a -> a -> (a,a) -> Maybe (a,a)
bill m x (a,b)
  | b > m = Nothing
  | a+b <= m && 10000*a + 5000*b + 1000*(m-a-b) == x = Just (a,b)
  | a+b > m = bill m x (0,b+1)
  | otherwise = bill m x (a+1,b)
