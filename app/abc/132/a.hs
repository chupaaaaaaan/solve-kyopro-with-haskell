import Data.List

main :: IO ()
main = do
  [a,b,c,d] <- sort <$> getLine
  putStrLn $ if a == b && c == d && b /= c
             then "Yes"
             else "No"
