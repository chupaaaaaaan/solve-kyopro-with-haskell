import Control.Monad

main :: IO ()
main = do
  [a,b] <- replicateM 2 (read <$> getLine) :: IO [Int]
  putStrLn . show $ min ((a - b) `mod` 10) (10 - (a - b) `mod` 10)
