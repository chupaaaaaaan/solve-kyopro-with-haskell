main :: IO ()
main = read <$> getLine >>= \n -> map read . words <$> getLine >>= \(t:a:_) -> snd . minimum . flip zip [1..n] . map (abs . (a-) . (t-) . (*) 0.006 . read) . words <$> getLine >>= print
