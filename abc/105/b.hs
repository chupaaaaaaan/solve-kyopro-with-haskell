main :: IO ()
main = do
  n <- readLn :: IO Int
  putStrLn $ if elem n . concat . map (\x -> filter (<=100) $ take (x+1) $ iterate (+3) (4*x)) $ [1..25]
    then "Yes"
    else "No"
