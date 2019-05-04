import Control.Monad
import Data.List
import Data.Maybe

main :: IO ()
main = do
  [a,b] <- map read . words <$> getLine
  print $ if abs (a - b) > 1 then 2 * max a b - 1 else a + b
