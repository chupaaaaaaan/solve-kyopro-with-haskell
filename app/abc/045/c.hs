main :: IO ()
main = do
  s <- getLine
  print . sum . map (sum . map (read :: String -> Int) . words . filter (/='@') . concat . zipWith (\x y -> [y,x] ) s . ('@':)) . (!!(length s - 1)) . iterate ([(' ':), ('@':)] <*>) $ [[]]
