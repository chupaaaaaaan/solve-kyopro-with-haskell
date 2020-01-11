import Control.Monad
import Control.Monad.ST
import Data.List
import Data.Maybe
import Data.Array.ST
import Data.Array.Unboxed

main :: IO ()
main = do
  s <- getLine
  n <- readLn :: IO Int
  (h,w) <- (\[x,y] -> (x,y)) . map read . words <$> getLine :: IO (Int,Int)
  as <- map read . words <$> getLine :: IO [Int]
  ss <- replicateM n getLine

  
  return ()
