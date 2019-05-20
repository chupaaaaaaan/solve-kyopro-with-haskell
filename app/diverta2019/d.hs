main :: IO ()
main = do
  n <- readLn :: IO Integer
  let divid = map (subtract 1) . concatMap (\x -> [x,n`div`x]) . filter ((==0) . (n`mod`)) $ [1..(floor . sqrt . fromIntegral $ n)]
  print $ sum $ filter (\x -> n`div`x == n`mod`x) $ filter (/=0) divid
