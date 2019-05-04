main :: IO ()
main = read . filter (/='/') <$> getLine >>= \s -> putStrLn $ if s <= 20190430 then "Heisei" else "TBD"
