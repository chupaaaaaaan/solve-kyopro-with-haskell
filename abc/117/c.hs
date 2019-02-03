import Data.List

main :: IO ()
main = do
  [n,_] <- map read . words <$> getLine :: IO [Int]
  xs <- sort . map read . words <$> getLine :: IO [Int]
  let zs = sum $ take (n - 1) $ sortBy (\a b -> compare b a) $ zipWith (-) (tail xs) xs
  print $ (last xs - head xs) - zs 
