main :: IO ()
main = getLine >>= \[a,b,c,d] -> putStrLn $ if b==c && (a==b || c==d) then "Yes" else "No"
