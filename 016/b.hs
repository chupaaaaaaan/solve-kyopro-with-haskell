main :: IO ()
main = do
  a:b:c:_ <- (map read . words <$> getLine) :: IO [Int]
  putStrLn (judge a b c)

judge :: Int -> Int -> Int -> String
judge x y z
  | y == 0 && x == z = "?"
  | x + y == z = "+"
  | x - y == z = "-"
  | otherwise = "!"
