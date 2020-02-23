module My.Algorithm.StdIn where

import           Control.Monad
import           Data.Array.Unboxed    (UArray)
import qualified Data.Array.Unboxed    as U
import qualified Data.ByteString.Char8 as BS
import           Data.Char
import           Data.List

getAsInt :: IO [Int]
getAsInt =  unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

getAsIntLine :: Int -> IO [[Int]]
getAsIntLine n = replicateM n getAsInt

getAsIntArray1Dr :: Int -> IO (UArray Int Int)
getAsIntArray1Dr n = U.listArray (1,n) <$> getAsInt

getAsIntArray1Dc :: Int -> IO (UArray Int Int)
getAsIntArray1Dc n = U.listArray (1,n) . concat <$> getAsIntLine n

-- n: number of lines
-- m: number of values per a line
getAsIntArray2D :: Int -> Int -> IO (UArray (Int,Int) Int)
getAsIntArray2D n m = U.listArray ((1,1),(n,m)) . concat <$> getAsIntLine n

getAsString :: IO [String]
getAsString = map BS.unpack . BS.words <$> BS.getLine
