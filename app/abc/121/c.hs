import Control.Monad
import Data.List

main :: IO ()
main = do
  [n,m] <- map read . words <$> getLine
  abn <- replicateM n $ (\[x,y] -> (x,y)) . map read . words <$> getLine :: IO [(Int,Int)]
  print . fst . foldl' (sum' m) (0,0) . sort $ abn

sum' :: Integral a => a -> (a,a) -> (a,a) -> (a,a)
sum' m' (p,q) (x,y)
  | q >= m' = (p,q)
  | (q + y) <= m' = (p + x * y, q + y)
  | otherwise = (p + x * (m' - q), m')

