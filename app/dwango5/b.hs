import Data.Bits
import Data.List
import qualified Data.Set as S

main :: IO ()
main = do
  [n,k] <- map read . words <$> getLine :: IO [Int]
  beauty <- sort . map sum . tail . subsequences . map read . words <$> getLine :: IO [Int]
  print . foldr1 (.&.) . drop (length beauty - k) $ beauty
