main :: IO ()
main = readLn >>= \x -> putStrLn $ if elem x [3,5,7] then "YES" else "NO"
