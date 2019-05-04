import Control.Monad
import Data.List

main :: IO ()
main = do
  [n,m] <- map read . words <$> getLine :: IO [Int]
  as <- replicateM n getLine
  bs <- replicateM m getLine

  putStrLn $ if isMatch n m as bs then "Yes" else "No"

isMatch :: Eq a => Int -> Int -> [[a]] -> [[a]] -> Bool
isMatch n m as bs
  | n < m = False
  | otherwise = if isInfixOf bs $ map (take m . drop (n-m)) as
                then True
                else isMatch (n-1) m as bs
