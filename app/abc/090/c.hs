main :: IO ()
main = map read . words <$> getLine >>= \(n:m:_) -> print . abs $ (n-2)*(m-2)
