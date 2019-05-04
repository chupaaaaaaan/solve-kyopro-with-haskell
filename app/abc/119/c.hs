import Control.Monad

main :: IO ()
main = do
  [n,a,b,c] <- map read . words <$> getLine
  ls <- map (cost (a,b,c)) . filter yyy . foldr xxx [[(0,0),(0,0),(0,0),(0,0)]] . map read <$> replicateM n getLine
  print $ minimum ls

xxx :: Int -> [[(Int,Int)]] -> [[(Int,Int)]]
xxx _ [] = []
xxx v ([(x0,x1),(y0,y1),(z0,z1),(u0,u1)]:xs) =
  [(x0+v,x1+1),(y0,y1),(z0,z1),(u0,u1)]
  : [(x0,x1),(y0+v,y1+1),(z0,z1),(u0,u1)]
  : [(x0,x1),(y0,y1),(z0+v,z1+1),(u0,u1)]
  : [(x0,x1),(y0,y1),(z0,z1),(u0+v,u1+1)] : xxx v xs


yyy :: [(Int,Int)] -> Bool
yyy [(_,x),(_,y),(_,z),_] = (x /= 0) && (y /= 0) && (z /= 0)

cost :: (Int,Int,Int) -> [(Int,Int)] -> Int
cost (x,y,z) [(x0,x1),(y0,y1),(z0,z1),_] =
  (max 0 (x1-1)) * 10 + abs (x0 - x) + (max 0 (y1-1)) * 10 + abs (y0 - y) + (max 0 (z1-1)) * 10 + abs (z0 - z)
