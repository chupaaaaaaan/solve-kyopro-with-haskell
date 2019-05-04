main :: IO ()
main = do
  [a,b] <- map read . words <$> getLine :: IO [Int]
  let ns = map show [a..b]
      ms = map reverse ns
  print $ length . filter id $ zipWith (==) ns ms
      
