import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = do
  [n,k] <- map read . words <$> getLine :: IO [Int]
  vec <- V.fromList . map ((\p -> (1 + p) / 2.0) . read) . words <$> getLine :: IO (Vector Double)
  let first = V.sum $ V.slice 0 k vec
  print $ solve n k vec 0 k first first

solve :: Int -> Int -> Vector Double -> Int -> Int -> Double -> Double -> Double
solve n k vec start end now maxim
  | end >= V.length vec = maxim
  | otherwise = let next = now - (vec V.! start) + (vec V.! end)
                in if maxim <= next
                   then solve n k vec (start+1) (end+1) next next
                   else solve n k vec (start+1) (end+1) next maxim
