import Control.Monad

main :: IO ()
main = do
  n <- readLn
  tas <- replicateM n $ (\[x,y] -> (x,y)) . map read . words <$> getLine
  print $ scanl1 solve tas


solve :: (Int,Int) -> (Int,Int) -> (Int,Int)
solve (a1,a2) (x,y)
  | sxy > 0 = (na1x`div`g12x,na2x`div`g12x)
  | sxy < 0 = (na1y`div`g12y,na2y`div`g12y)
  | otherwise = (a1,a2)
  where sxy = a2*x - a1*y
        cofx = x `div` gcd a1 x
        cofy = y `div` gcd a2 y
        na1x = a1*cofy+(sxy*cofy)`div`y
        na2x = a2*cofy
        na1y = a1*cofx
        na2y = a2*cofx+(-sxy*cofx)`div`x
        g12x = gcd na1x na2x
        g12y = gcd na1y na2y


-- (x, y, d) = extgcd a b
-- d == gcd a b
-- d == a * x + b * y
extgcd :: Int -> Int -> (Int,Int,Int)
extgcd = go 1 0 0 1
  where go x y _ _ d 0 = (d, x, y)
        go x y x' y' a b = let q = a`div`b
                               r = a`mod`b
                           in go x' y' (x-q*x') (y-q*y') b r
