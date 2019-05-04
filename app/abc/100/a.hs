main :: IO ()
main = map read . words <$> getLine >>= \[a,b] -> putStrLn $ if a<=8 && b<=8 then "Yay!" else ":("
