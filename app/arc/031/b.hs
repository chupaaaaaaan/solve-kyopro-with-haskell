import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.List
import Data.Maybe

type Idx = (Int,Int)
main :: IO ()
main = do
  cs <- concat <$> replicateM 10 getLine
  putStrLn $ if solve cs then "Yes" else "No"

solve :: String -> Bool
solve xs = runST $ do
  ary <- newListArray ((0,0),(9,9)) xs :: ST s (STUArray s Idx Char)
  go [(si,sj)] ary
  where go :: [Idx] -> STUArray s Idx Char -> ST s Bool
        go [] _ = return False
        go ((i,j):is) ar
          | i<0 || i>=h || j<0 || j>=w = go is ar
          | otherwise = do
              pt <- readArray ar (i,j)
              case pt of
                '#' -> go is ar
                'g' -> return True
                _   -> writeArray ar (i,j) '#' >> go ((i-1,j):(i+1,j):(i,j-1):(i,j+1):is) ar
