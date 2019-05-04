main :: IO ()
main = readLn >>= \n -> print $ (floor (sqrt n))^2
