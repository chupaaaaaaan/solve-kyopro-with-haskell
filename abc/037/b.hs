import Control.Monad

main :: IO ()
main = do
  [n,q] <- map read . words <$> getLine
  lrts <- replicateM q (words <$> getLine)
  let ls = map (read . (flip (!!) 0)) lrts :: [Int]
      rs = map (read . (flip (!!) 1)) lrts :: [Int]
      ts = map (read . (flip (!!) 2)) lrts :: [Integer]
      lrtt = zip3 ls rs ts
  putStr . unlines . map show $ foldl translate (replicate n 0) lrtt

translate :: [Integer] -> (Int,Int,Integer) -> [Integer]
translate xs (l,r,t) = take (l-1) xs ++ replicate (r - l + 1) t ++ drop r xs
