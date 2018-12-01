main :: IO ()
main = do
  n <- getLine
  let f = replicate (length n) [('7':),('5':),('3':)]
  print $ length . filter (<=(read n :: Int)) . map read . filter (\x -> all (flip elem x) "357") . concat . scanr (<*>) [[]] $ f




