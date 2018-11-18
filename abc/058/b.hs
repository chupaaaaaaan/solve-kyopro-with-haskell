main :: IO ()
main = getLine >>= \o -> getLine >>= \e -> putStrLn $ f o e

f :: [a] -> [a] -> [a]
f [] [] = []
f [a] [] = [a]
f (a:as) (b:bs) = a:b:(f as bs)
