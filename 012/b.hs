main :: IO ()
main = do
  n <- (read <$> getLine) :: IO Int
  putStrLn $ showIF (n `div` 3600) ++ ":" ++ showIF ((n `mod` 3600) `div` 60) ++ ":" ++ showIF (n `mod` 60)

showIF :: Int -> String
showIF n
  | n < 10 = '0' : show n
  | otherwise = show n
