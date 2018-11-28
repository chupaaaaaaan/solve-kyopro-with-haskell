import Data.List
main :: IO ()
main = map read . words <$> getLine >>= \(n:k:_) -> sum . drop (n - k) . sort . map read . words <$> getLine >>= print
