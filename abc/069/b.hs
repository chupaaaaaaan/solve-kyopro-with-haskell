main :: IO ()
main = getLine >>= \s -> putStrLn $ [head s] ++ show (length s - 2) ++ [last s]
