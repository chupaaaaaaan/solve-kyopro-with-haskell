import Data.List
main :: IO ()
main = sort <$> getLine >>= \s -> reverse . sort <$> getLine >>= \t -> putStrLn $ if s < t then "Yes" else "No"
