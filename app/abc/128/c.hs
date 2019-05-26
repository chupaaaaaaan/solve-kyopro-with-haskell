import Control.Monad


main :: IO ()
main = do
  [n,m] <- map read . words <$> getLine :: IO [Int]
  kss <- replicateM m $ map read . words <$> getLine :: IO [[Int]]
  ps <- map read . words <$> getLine :: IO [Int]

  let ios = (!!n) $ iterate ([(0:),(1:)] <*>) [[]]

  print $ solve ios kss ps m

solve :: [[Int]] -> [[Int]] -> [Int] -> Int -> Int
solve ios kss ps m = go ios kss ps 0 0
  where go [] _ _ _ b = b
        go (io:ios') ((_:s):kss') (p:ps') acc bcc = let l = (`mod`2) $ sum $ map ((io!!) . subtract 1) s
                                                    in if l == p
                                                       then go (io:ios') kss' ps' (acc+1) bcc
                                                       else go (io:ios') kss' ps' acc     bcc
        go (io:ios') [] [] acc bcc = if acc == m then go ios' kss ps 0 (bcc+1) else go ios' kss ps 0 bcc
