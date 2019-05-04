main :: IO ()
main = do
  n <- readLn :: IO Int
  let k = powerset [3,5,7,11,13,17]
      a  = filter (<=n) $ map product $ filter ((==3) . length) k
      b1 = filter (<=n) $ map (\[x,y] -> x^3*y) $ filter ((==2) . length) k
      -- b2 = filter (<=n) $ map (\[x,y] -> y^3*x) $ filter ((==2) . length) k
      -- c  = filter (<=n) $ map (\[x] -> x^7) $ filter ((==1) . length) k
  print $ length $ a ++ b1

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = [x:ys | ys <- powerset xs] ++ powerset xs
