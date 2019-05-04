import Control.Monad
main :: IO ()
main = do
  n <- read <$> getLine
  lrs <- map (\x -> (,) (x!!0) (x!!1)) <$> replicateM n (map read . words <$> getLine) :: IO [(Int,Int)]
  print $ sum . map (\x -> snd x - fst x + 1) $ lrs
  
