main :: IO ()
main = read <$> getLine >>= print . (^) 4
