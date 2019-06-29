main :: IO ()
main = do
  getLine
  ps <- map read <$> words <$> getLine :: IO [Int]
  print $ sum $ solve ps

solve :: [Int] -> [Int]
solve [] = []
solve [a] = []
solve [a,b] = []
solve (a:b:c:as) = case (compare a b, compare b c) of
                     (LT,LT) -> 1 : solve (b:c:as)
                     (GT,GT) -> 1 : solve (b:c:as)
                     (LT,EQ) -> 1 : solve (b:c:as)
                     (EQ,GT) -> 1 : solve (b:c:as)
                     _ -> 0 : solve (b:c:as)
