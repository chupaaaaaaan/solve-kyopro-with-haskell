import Control.Monad

main :: IO ()
main = do
  [h,w] <- map read . words <$> getLine
  as <- replicateM h getLine
  putStrLn $ replicate (w+2) '#'
  forM_ as $ \x -> do
    putChar '#'
    putStr x
    putStrLn "#"
  putStrLn $ replicate (w+2) '#'
    
