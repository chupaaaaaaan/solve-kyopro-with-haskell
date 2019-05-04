import Control.Monad

main :: IO ()
main = do
  n <- (read <$> getLine) :: IO Int
  t <- (map read <$> replicateM n getLine) :: IO [Int]
  putStrLn $ show $ foldr min 101 t
  
