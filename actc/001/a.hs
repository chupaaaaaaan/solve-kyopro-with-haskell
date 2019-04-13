import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.List
import Data.Maybe

type Idx = (Int,Int)
main :: IO ()
main = do
  [h,w] <- map read . words <$> getLine
  cs <- concat <$> replicateM h getLine
  let s =  (\x -> (x`mod`h,x`div`h)) . fromJust . elemIndex 's' $ cs
  putStrLn $ if solve cs s (h,w) then "Yes" else "No"

solve :: String -> Idx -> Idx -> Bool
solve xs (si,sj) (h,w) = runST $ do
  ary <- newListArray ((0,0),(h-1,w-1)) xs :: ST s (STUArray s Idx Char)
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
