main :: IO ()
main = do
  [k,s] <- map read . words <$> getLine :: IO [Int]
  print $ length [(x,y)|x<-[(max 0 (s-2*k))..k],y<-[(max 0 (s-2*k))..k],x+y>=s-k,x+y<=s]

{-
z = s - x - y <= k

x + y <= s

x + y >= s - k

k >= y >= s - k - x >= s - 2k

-}
