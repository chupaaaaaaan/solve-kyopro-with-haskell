import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- readLn :: IO Integer
  as <- replicateM 5 readLn :: IO [Integer]
  let (b:_) = sort as
  print $ (n-1)`div`b + 5
