import Control.Monad
main :: IO ()
main = do
  [n,m] <- map read . words <$> getLine
  ab <- concat <$> replicateM m (map read . words <$> getLine)
  mapM_ print $ map (length . flip filter ab . (==)) [1..n]
