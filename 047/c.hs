import Data.List
main :: IO ()
main = getLine >>= \s -> print $ length (group s) - 1
