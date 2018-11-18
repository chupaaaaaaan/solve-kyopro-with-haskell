main :: IO ()
main = do
  n <- read <$> getLine
  let lat = [(x,y) | x <- [0..n-1], y <- [0..n-1]]
  print . length . filter (isInternalBox n) $ lat

isInternalPoint :: Int -> (Int,Int) -> Bool
isInternalPoint c (x,y) = if (a+b-0.5*d>=0) && (a-b+0.5*d>=0) && (a-b-0.5*d<=0) && (a+b-1.5*d<=0) then True else False
  where a = fromIntegral x
        b = fromIntegral y
        d = fromIntegral c

isInternalBox :: Int -> (Int,Int) -> Bool
isInternalBox c (x,y) = isInternalPoint c (x,y) &&
                        isInternalPoint c (x+1,y) &&
                        isInternalPoint c (x,y+1) &&
                        isInternalPoint c (x+1,y+1)
