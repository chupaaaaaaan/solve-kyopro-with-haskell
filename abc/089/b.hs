main :: IO ()
main = getLine >> elem 'Y' . concat . words <$> getLine >>= \b -> putStrLn $ if b then "Four" else "Three"
