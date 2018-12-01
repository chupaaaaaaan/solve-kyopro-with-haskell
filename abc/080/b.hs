main :: IO ()
main = getLine >>= \n -> putStrLn $ if (read n) `mod` (sum . map (read . (:[])) $ n) == 0 then "Yes" else "No"
