main :: IO ()
main = minimum . map (abs . subtract 753 . read) . f <$> getLine >>= print

f :: [a] -> [[a]]
f [] = []
f [_] = []
f [_,_] = []
f (a:b:c:as) = (a:b:c:[]):f (b:c:as)
