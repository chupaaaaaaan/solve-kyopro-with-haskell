main :: IO ()
main = getLine >>= putStrLn . f

f :: [a] -> [a]
f [] = []
f [x] = [x]
f (x:_:xs) = x:f xs
