import Control.Monad

main :: IO ()
main = do
  [n,q] <- map read . words <$> getLine :: IO [Int]
  lr <- replicateM q (map read . words <$> getLine) :: IO [[Int]]
  let osero = replicate n False

  putStrLn $ map g . foldl f osero $ lr




f :: [Bool] -> [Int] -> [Bool]
f xs [l,r] = take (l-1) xs ++ (map not . drop (l-1) . take r) xs ++ drop r xs

g :: Bool -> Char
g x
  | x == True = '1'
  | otherwise = '0'
