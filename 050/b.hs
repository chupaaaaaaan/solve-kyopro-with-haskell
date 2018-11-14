import Control.Monad

main :: IO ()
main = do
  _ <- read <$> getLine :: IO Int
  ts <- map read . words <$> getLine :: IO [Int]
  m <- read <$> getLine :: IO Int
  pxs <- replicateM m (map read . words <$> getLine) :: IO [[Int]]
  let ps = map (!!0) pxs
      xs = map (!!1) pxs
      pxt = zip ps xs
      sumts = sum ts
  forM_ pxt $ \px -> do
    print $ sumts - (ts!!(fst px - 1) - (snd px))
