import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- read <$> getLine :: IO Int
  rs <-  reverse . sort . map read <$> replicateM n getLine :: IO [Double]
  print $ pi * (sum $ area rs)

area :: [Double] -> [Double]
area [] = []
area [x] = [x**2]
area (x:y:xs) = (x**2 - y**2) : area xs
