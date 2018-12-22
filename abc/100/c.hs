main :: IO ()
main = getLine >> sum . map (f . read) . words <$> getLine >>= print

f :: Int -> Int
f x = if even x then 1 + f (x`div`2) else 0
