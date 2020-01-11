import Control.Monad

main :: IO ()
main = do
  n <- readLn :: IO Int
--   xys <- (\[x,y] -> (x,y)) . replicateM n . map read . words <$> getLine :: IO [(Int,Int)]
  xys <- map ((\[x,y] -> (x,y)) . map read . words) <$> replicateM n getLine :: IO [(Int,Int)]










