import Data.List
import Data.Maybe
import Control.Monad
import Data.Array.Unboxed
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
  n <- readLn :: IO Int
  ds <- listArray (0,n-1) . sort . map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine :: IO (UArray Int Int)
  let a = ds!(n`div`2 - 1)
      b = ds!(n`div`2)
  print $ b - a
  
