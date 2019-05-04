main :: IO ()
main = do
  _ <- getLine
  as <- map read . words <$> getLine :: IO [Integer]
  let (c,a,b) = solve as 0 0 1000000000
  print $ if even c then a else a - 2 * b

solve :: [Integer] -> Int -> Integer -> Integer -> (Int,Integer,Integer)
solve [] c a b = (c,a,b)
solve (x:xs) c a b
  | x >= 0 && x < b = solve xs c (a + x) x
  | x >= 0 && x >= b = solve xs c (a + x) b
  | x < 0 && (-x) < b = solve xs (c + 1) (a - x) (-x)
  | x < 0 && (-x) >= b = solve xs (c + 1) (a - x) b
