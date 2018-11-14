main :: IO ()
main = do
  sa <- getLine
  sb <- getLine
  sc <- getLine
  putStrLn $ solve 'a' sa sb sc

solve :: Char -> String -> String -> String -> String
solve 'a' [] _ _ = "A"
solve 'b' _ [] _ = "B"
solve 'c' _ _ [] = "C"
solve x as bs cs
  | x == 'a' = solve (head as) (tail as) bs cs
  | x == 'b' = solve (head bs) as (tail bs) cs
  | x == 'c' = solve (head cs) as bs (tail cs)
