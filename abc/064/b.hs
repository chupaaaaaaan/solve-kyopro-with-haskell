main :: IO ()
main = getLine >> map read . words <$> getLine >>= \as -> print $ (maximum as) - (minimum as)
