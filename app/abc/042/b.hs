import Control.Monad
import Data.List
main :: IO ()
main = do
  [n,_] <- map read . words <$> getLine :: IO [Int]
  ss <- replicateM n getLine
  putStrLn $ concat . sort $ ss
