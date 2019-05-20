import Numeric

main :: IO ()
main = do
  [n,k] <- map read . words <$> getLine :: IO [Double]
  if n < k
    then print . (/n)              . sum . map ((0.5^) . ceiling . logBase 2 . (k/)) $ [1..n]
    else print . (/n) . (+(n-k+1)) . sum . map ((0.5^) . ceiling . logBase 2 . (k/)) $ [1..(k-1)]
