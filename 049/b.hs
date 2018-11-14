import Control.Monad

main :: IO ()
main = do
  [h,_] <- map read . words <$> getLine :: IO [Int]
  cs <- replicateM h getLine
  putStrLn . unlines . concat $ zipWith (\x -> \y -> x:y:[]) cs cs
