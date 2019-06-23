import Data.List
import Data.Maybe
import Control.Monad
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
  n <- readLn :: IO Int
  abz <- replicateM n $ (\[x,y] -> (x,y)) . map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
  let abz' = sortBy (\(x,y) (z,u) -> compare u y <> compare z x) abz

  putStrLn $ solve abz'

solve :: [(Int,Int)] -> String
solve [] = "Yes"
solve [(x,y)] = if y - x >= 0 then "Yes" else "No"
solve ((x,y):(z,u):xyz) = if y - x >= u
                          then solve ((z,u):xyz)
                          else solve ((z,y-x):xyz)
