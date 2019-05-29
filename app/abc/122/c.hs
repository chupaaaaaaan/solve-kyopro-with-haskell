import Control.Monad
import Data.Array.Unboxed

main :: IO ()
main = do
  [n,q] <- map read . words <$> getLine
  s <- getLine
  lrs <- replicateM q $ (\[x,y] -> (x,y)) . map read . words <$> getLine :: IO [(Int,Int)]
  let acs = listArray (0,n-1) $ 0 : neighbour s 0 :: UArray Int Int
  forM_ lrs $ \(l,r) -> print $ acs!(r-1) - acs!(l-1)

neighbour :: String -> Int -> [Int]
neighbour [] _ = []
neighbour [_] _ = []
neighbour (x:y:xs) acc
  | [x,y] == "AC" = (acc + 1) : neighbour (y:xs) (acc + 1)
  | otherwise     = acc       : neighbour (y:xs) acc

{-
12345678
ACACTACG
10100100

01122233

3 7 -> 2
2 3 -> 0
1 8 -> 3

-}
