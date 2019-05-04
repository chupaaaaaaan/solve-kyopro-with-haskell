main :: IO ()
main = map read . words <$> getLine >>= \[d,n] -> print . (*(100^d)) . (!!(n-1)) $ [1..99]++[101]
