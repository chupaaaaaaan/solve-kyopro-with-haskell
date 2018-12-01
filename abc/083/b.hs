main :: IO ()
main = do
  [n,a,b] <- map read . words <$> getLine :: IO [Int]
  print . sum . map fst . filter (\x -> (snd x>=a) && (snd x<=b)) . zip [1..n] . map (sum . map (read . (:[])) . show) $ [1..n]

