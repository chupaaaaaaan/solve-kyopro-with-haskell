import Data.List
main :: IO ()
main = do
  [a,b,c] <- sort . map read . words <$> getLine
  if a + b == c
    then putStrLn "Yes"
    else putStrLn "No"
