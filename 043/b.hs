main :: IO ()
main = do
  s <- getLine
  putStrLn $ reverse $ solve s []

solve :: String -> String -> String
solve [] str = str
solve (x:xs) ys
  | x == '0' || x == '1' = solve xs (x:ys)
  | x == 'B' && not (null ys) = solve xs (drop 1 ys)
  | x == 'B' && null ys = solve xs ys
