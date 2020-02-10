import Control.Monad

main :: IO ()
main = do
  n <- readLn
  tas <- replicateM n $ (\[x,y] -> (x,y)) . map read . words <$> getLine
  let (x,y) = foldl1 solve tas
  print $ x + y

solve :: (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer)
solve (x,y) (t,a) = let x' = fromInteger x
                        y' = fromInteger y
                        t' = fromInteger t
                        a' = fromInteger a
                        f = max ((x' + t' - 1) `div` t') ((y' + a' - 1) `div` a')
                    in (f * t, f * a)
