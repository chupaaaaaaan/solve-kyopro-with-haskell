main :: IO ()
main = do
  _ <- getLine
  ws <- map read . words <$> getLine
  print $ solve ws $ sum ws

solve :: [Int] -> Int -> Int
solve []  a = a
solve (x:xs) a
  | abs a > abs (a - 2*x) = solve xs (a - 2*x)
  | otherwise = abs a

