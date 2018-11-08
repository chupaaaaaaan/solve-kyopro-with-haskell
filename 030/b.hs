main :: IO ()
main = do
  [n,m] <- map read . words <$> getLine :: IO [Int]
  let cn = 0.5 * fromIntegral m + 30 * fromIntegral (n `mod` 12)
      cm = 6 * fromIntegral m :: Double
  if cn < cm
    then print $ min (cm - cn) (360 - (cm - cn))
    else print $ min (cn - cm) (360 - (cn - cm))
    
