main :: IO ()
main = getLine >>= \s -> getLine >>= \t -> putStrLn $ if t `elem` map (rot s) [0..(length s)] then "Yes" else "No"

rot :: [a] -> Int -> [a]
rot xs n = drop n xs ++ take n xs
