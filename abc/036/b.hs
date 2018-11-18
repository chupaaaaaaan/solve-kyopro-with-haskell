import Control.Monad

main :: IO ()
main = do
  n <- read <$> getLine :: IO Int
  ss <- replicateM n getLine
  mapM_ putStrLn $ rot90 ss

rot90 :: [[a]] -> [[a]]
rot90 sss
  | or $ map null sss = []
  | otherwise = (reverse $ map head sss) : (rot90 $ map tail sss)
