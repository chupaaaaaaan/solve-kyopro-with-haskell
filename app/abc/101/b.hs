main :: IO ()
main = do
  n <- getLine
  let sn = sum . map (read . (:[])) $ n
      nn = read n
  putStrLn $ if nn`mod`sn==0 then "Yes" else "No"
