import Control.Monad

main :: IO ()
main = do
  [d,g] <- map read . words <$> getLine
  pcs <- replicateM d $ (\[x,y] -> (x,y)) . map read . words <$> getLine
  
