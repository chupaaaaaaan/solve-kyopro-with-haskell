import Control.Monad

main :: IO ()
main = do
  [n,_,c] <- map read . words <$> getLine
  bm <- map read . words <$> getLine
  an <- replicateM n $ sum . zipWith (*) bm . map read . words <$> getLine
  print . length . filter (\x -> x > (-c)) $ an
