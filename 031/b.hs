import Control.Monad
main :: IO ()
main = do
  [l,h] <- map read . words <$> getLine :: IO [Int]
  n <- read <$> getLine :: IO Int
  as <- map read <$> replicateM n getLine :: IO [Int]
  forM_ as $ \s -> do
    if s < l
      then print $ l - s
      else if s > h
           then putStrLn "-1"
           else putStrLn "0"

  
