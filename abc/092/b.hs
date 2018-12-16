import Control.Monad

main :: IO ()
main = do
  n <- readLn
  [d,x] <- map read . words <$> getLine
  as <- sum . map ((+1) . (`div`d) . subtract 1 . read) <$> replicateM n getLine
