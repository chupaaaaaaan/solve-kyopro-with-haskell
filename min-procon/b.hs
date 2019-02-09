import Data.List
main :: IO ()
main = do
  a <- map read . words <$> getLine :: IO [Int]
  b <- map read . words <$> getLine
  c <- map read . words <$> getLine
  putStrLn $ if (maximum . map length . group . sort . concat $ [a,b,c]) == 3
             then "NO"
             else "YES"

