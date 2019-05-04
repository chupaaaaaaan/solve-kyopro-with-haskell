import Control.Monad

main :: IO ()
main = do
  [w,h,n] <- map read . words <$> getLine
  xyas <- replicateM n $ map read . words <$> getLine :: IO [[Int]]
  let xyat = zip3 (map (!!0) xyas) (map (!!1) xyas) (map (!!2) xyas)
      x1 = maximum . (0:) . map fst3 . filter ((==) 1 . thd3) $ xyat
      x2 = minimum . (w:) . map fst3 . filter ((==) 2 . thd3) $ xyat
      y1 = maximum . (0:) . map snd3 . filter ((==) 3 . thd3) $ xyat
      y2 = minimum . (h:) . map snd3 . filter ((==) 4 . thd3) $ xyat
  print $ (max (x2 - x1) 0) * (max (y2 - y1) 0)

fst3,snd3,thd3 :: (a,a,a) -> a
fst3 (x,_,_) = x
snd3 (_,y,_) = y
thd3 (_,_,z) = z

