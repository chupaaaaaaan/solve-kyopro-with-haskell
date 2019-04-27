import Data.List

main :: IO ()
main = do
  _ <- getLine
  as <-  map read . words <$> getLine :: IO [Int]
  let gcdr = scanr gcd 0 as
      gcdl = scanl' gcd 0 as
  print $ maximum $ zipWith gcd gcdl (tail gcdr)
