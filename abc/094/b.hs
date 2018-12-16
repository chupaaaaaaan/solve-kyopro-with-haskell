main :: IO ()
main = do
  _:_:x:_ <- map read . words <$> getLine :: IO [Int]
  (a,b) <- span (<x) . map read . words <$> getLine
  print $ min (length a) (length b)
