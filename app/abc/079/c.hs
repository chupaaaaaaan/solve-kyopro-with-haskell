main :: IO ()
main = do
  abcd <- getLine
  putStrLn . head . filter (not . null) . map (solve . tail . concat . zipWith (\x y -> [y,x]) abcd . ('@':)) . foldr (<*>) [[]] . replicate (length abcd -1 ) $ [('+':),('-':)]


solve :: String -> String
solve xs = go xs 0
  where go :: String -> Int -> String
        go [] acc = if acc == 7 then xs ++ "=7" else []
        go [_] _ = []
        go (a:b:as) acc
          | a == '+' = go as (acc + readInt [b])
          | a == '-' = go as (acc - readInt [b])
          | otherwise = go (b:as) (readInt [a])

readInt :: String -> Int
readInt = read
