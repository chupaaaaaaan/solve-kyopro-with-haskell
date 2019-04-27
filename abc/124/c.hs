main :: IO ()
main = do
  s <- map (=='1') <$> getLine
  print $ min (solveOdd True s 0 0) (solveOdd False s 0 0)

solveOdd :: Bool -> [Bool] -> Int -> Int -> Int
solveOdd _ [] acco acce = acco + acce
solveOdd b (x:xs) acco acce
  | x == b = solveEven (not b) xs (acco + 1) acce
  | otherwise = solveEven (not b) xs acco acce

solveEven :: Bool -> [Bool] -> Int -> Int -> Int
solveEven _ [] acco acce = acco + acce
solveEven b (x:xs) acco acce
  | x == b = solveOdd (not b) xs acco (acce + 1)
  | otherwise = solveOdd (not b) xs acco acce
