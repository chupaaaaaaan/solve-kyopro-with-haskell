main :: IO ()
main = do
  [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z] <- map read . words <$> getLine
  print $ a - b
  print $ c + d
  print $ max (f - e + 1) 0
  print $ (g + h + i) `div` 3 + 1
  putStrLn $ case j of
               0 -> ""
               1 -> "a"
               2 -> "aa"
               3 -> "aaa"
               4 -> "aaai"
               5 -> "aaaji"
               6 -> "aabaji"
               7 -> "agabaji"
               8 -> "dagabaji"
  let mm = (30*59*l - 29*61*k)`mod`(59*61)
      nn = 59*61*(m - 1) + mm
      nn2 = snd $ head $ filter (\(xx,_) -> xx >= n) $ zipWith (\xx yy -> (abs (nn - xx),yy)) perfects perfects
  print $ min nn nn2
  print $ max nn nn2
  print $ ((o + p + q) * (r + s + t) * (u + v + w) * (x + y + z))`mod`9973


perfects :: [Int]
perfects = [6,28,496,8128,33550336,8589869056]






    -- m == k (mod 59)
    -- m == l (mod 61)
    -- 61m == 61k (mod 59*61)
    -- 59m == 59l (mod 59*61)
    -- 2m == (61k - 59l) (mod 59*61)
    -- 58m == 29*(61k - 59l) (mod 59*61)
    -- m == 59l - 29*(61k - 59l) (mod 59*61)
    --    = 30*59l - 29*61k
    --    = (30*59l - 29*61k)`mod`59*61
