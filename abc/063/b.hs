import Data.List
main :: IO ()
main = do
  s <- getLine
  putStrLn $ if (==) (length s) (length.nub $ s) then "yes" else "no"
