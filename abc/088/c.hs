main :: IO ()
main = do
  [c11,c12,c13] <- map read . words <$> getLine
  [c21,c22,c23] <- map read . words <$> getLine
  [c31,c32,c33] <- map read . words <$> getLine
  
  putStrLn $ if c11-c21 == c12-c22 && c12-c22 == c13-c23 &&
                c21-c31 == c22-c32 && c22-c32 == c23-c33 &&
                c11-c12 == c21-c22 && c21-c22 == c31-c32 &&
                c12-c13 == c22-c23 && c22-c23 == c32-c33
             then "Yes"
             else "No"
