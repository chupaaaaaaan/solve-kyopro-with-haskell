import Control.Monad

main :: IO ()
main = do
  [h,w] <- map read . words <$> getLine
  ss <- replicateM h getLine

  putStr $ unlines [map (f ss x) [1..w] | x <- [1..h]]

window2d :: [[a]] -> Int -> Int -> [[a]]
window2d xs i j = map (take (3 - (max 0 (2-j))) . drop (j-2)) . take (3 - (max 0 (2-i))) . drop (i-2) $ xs

f :: [String] -> Int -> Int -> Char
f xs i j
  | (xs!!(i-1))!!(j-1) == '#' = '#'
  | otherwise = head . show . length . filter (=='#') . concat $ window2d xs i j
