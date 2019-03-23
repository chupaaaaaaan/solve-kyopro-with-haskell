main :: IO ()
main = do
  getLine
  as <- map read . words <$> getLine
  print $ minimum $ map (\x -> sum . map (\y -> (x - y)^2) $ as) [-100..100]
