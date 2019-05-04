import Control.Monad

main :: IO ()
main = do
  [_,q] <- map read . words <$> getLine
  s <- getLine
  lrs <- replicateM q $ (\[x,y] -> (x,y)) . map read . words <$> getLine :: IO [(Int,Int)]
  let acs = neighbour s 0
  print acs
  forM_ lrs $ \(l,r) -> case l of
    1 -> print $ acs!!(r-2)
    2 -> print $ acs!!(r-2) - acs!!0
    _ -> print $ acs!!(r-2) - acs!!(l-1)

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

1122233_





3 7 -> 2
2 3 -> 0
1 8 -> 3

-}
