import Control.Monad
main :: IO ()
main = do
  n <- readLn
  [d,x] <- map read . words <$> getLine
  print =<< (+x) . sum . map ((+1) . div (d-1) . read) <$> replicateM n getLine
