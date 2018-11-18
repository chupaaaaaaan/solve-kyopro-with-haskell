main :: IO ()
main = read <$> getLine >>= \n -> print . minimum . map (solve n) $ [1..n]

solve :: Int -> Int -> Int
solve n m = abs (l - m) + n - l * m
  where l = n `div` m
