import Control.Monad
import Data.List
main :: IO ()
main = do
  n <- readLn
  abt <- map ((\[x,y] -> (x,y)) . map read . words) <$> replicateM n getLine :: IO [(Int,Int)]
  cdt <- map ((\[x,y] -> (x,y)) . map read . words) <$> replicateM n getLine :: IO [(Int,Int)]
  print $ maximum . (0:) . map (length . nub) . sequence . map (\(a,b) -> filter (\(c,d) -> a < c && b < d) cdt) $ abt
