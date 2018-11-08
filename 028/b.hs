main :: IO ()
main = getLine >>= \s -> putStrLn . unwords . map (show .length . (\x -> filter (==x) s)) $ "ABCDEF"
