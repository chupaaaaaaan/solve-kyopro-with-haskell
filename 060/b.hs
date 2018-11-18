main :: IO ()
main = map read . words <$> getLine >>= \(a:b:c:_) -> if any (==c) . map (flip mod b . (*) a) $ [0..b-1] then putStrLn "YES" else putStrLn "NO"
