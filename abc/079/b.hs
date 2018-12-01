main :: IO ()
main = readLn >>= print . lucas 2 1

lucas :: (Integral a) => a -> a -> a -> a
lucas a _ 0 = a
lucas _ b 1 = b
lucas a b n = lucas b (a+b) (n-1)
