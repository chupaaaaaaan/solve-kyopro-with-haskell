main :: IO ()
main = map read . words <$> getLine >>= \[a,b] -> print $ ((b-a)^2 - (b+a)) `div` 2

