module My.Algorithm.ExtGcd where

-- (x, y, d) = extgcd a b
-- d := gcd a b
-- d == a * x + b * y
-- see https://blog.miz-ar.info/2017/09/euclidean-algorithm/

extgcd :: Int -> Int -> (Int,Int,Int)
extgcd = go 1 0 0 1
  where go x y _ _ d 0 = (d, x, y)
        go x y x' y' a b = let q = a`div`b
                               r = a`mod`b
                           in go x' y' (x-q*x') (y-q*y') b r

