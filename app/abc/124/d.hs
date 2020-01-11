main :: IO ()
main = do
  [n,k] <- map read . words <$> getLine :: IO [Int]
  s <- getLine
  let pr = grp s 0 0
  print pr
  print $ cum pr 0



grp :: String -> Int -> Int -> [Int]
grp [] _ _ = []
grp [x] a b
  | x == '1' = [a+1,0]
  | x == '0' = [a,b+1]
grp (x:y:ss) a b
  | x == '1'             = grp (y:ss) (a+1) b
  | x == '0' && y == '0' = grp (y:ss) a (b+1)
  | x == '0' && y == '1' = a : b+1 : grp (y:ss) 0 0

cum :: [Int] -> Int -> [Int]
cum [] _ = []
cum (x:xs) acc = (x + acc) : cum xs (x + acc)

