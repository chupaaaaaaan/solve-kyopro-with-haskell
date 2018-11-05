main :: IO ()
main = do
  [x, y] <- (map read . words <$> getLine) :: IO [Int]
  case x < y of
    True -> print y
    False -> print x
