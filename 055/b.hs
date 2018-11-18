main :: IO ()
main = read <$> getLine >>= \n -> print . foldr (\x y -> (x * y) `mod` (10^9 + 7)) 1 $ [1..n]


