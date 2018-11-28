main :: IO ()
main = do
  getLine
  k <- read <$> getLine
  xs <- map read . words <$> getLine
  print $ (*2) . sum . map (\x -> if x < k - x then x else k - x) $ xs
