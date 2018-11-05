main :: IO ()
main = do
  m <- (read <$> getLine) :: IO Int
  putStrLn $ vv m

vv :: Int -> String
vv n
  | n < 100 = "00"
  | n >= 100 && n <= 5000 = take 2 $ (if (length $ show n) == 3 then "0" else "") ++ (show n)
  | n >= 6000 && n <= 30000 = take 2 $ show (n + 50000)
  | n >= 35000 && n <= 70000 = take 2 $ show (80000 + (n - 30000) `div` 5)
  | n >= 70000 = "89"
