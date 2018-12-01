import Control.Monad

main :: IO ()
main = do
  [h,w] <- map read . words <$> getLine
  ss <- replicateM h getLine
  let ss1 = [replicate (w+2) '.'] ++ map (("."++) . (++".")) ss ++ [replicate (w+2) '.']

  putStrLn $ if and [f ss1 x y | x <- [2..h+1], y <- [2..w+1]] then "Yes" else "No"

f :: [String] -> Int -> Int -> Bool
f xs i j
  | (xs!!(i-1))!!(j-1) == '#' &&
    (xs!!i)!!(j-1)     == '.' &&
    (xs!!(i-2))!!(j-1) == '.' &&
    (xs!!(i-1))!!j     == '.' &&
    (xs!!(i-1))!!(j-2) == '.' = False
  | otherwise = True
