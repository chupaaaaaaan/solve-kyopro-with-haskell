import Data.Char

main :: IO ()
main = do
  [n,k] <- map read . words <$> getLine :: IO [Int]
  s <- getLine
  putStrLn $ take (k-1) s ++  [toLower (s!!(k-1))] ++ drop k s
  
