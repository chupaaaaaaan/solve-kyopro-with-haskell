main :: IO ()
main = getLine >> map read . words <$> getLine >>= print . f 0

f :: Int -> [Int] -> Int
f n as
  | all even as = f (n+1) $ map (`div`2) as
  | otherwise = n
