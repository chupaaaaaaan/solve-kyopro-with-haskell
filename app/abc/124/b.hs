import Control.Monad
import Data.List
import Data.Maybe

main :: IO ()
main = do
  n <- readLn
  ns <- map read . words <$> getLine :: IO [Int]
  print $ length . filter id . for [0..n-1] $ \x -> all (<=ns!!x) $ take (x+1) ns
  where for x y = map y x
