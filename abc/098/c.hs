main :: IO ()
main = getLine >> getLine >>= \s -> print . minimum . f s $ (length . filter (=='E') . tail $ s)

f :: String -> Int -> [Int]
f [_] a = [a]
f (s:t:ss) n
  | s == 'E' && t == 'E' = n : f (t:ss) (n-1)
  | s == 'W' && t == 'W' = n : f (t:ss) (n+1)
  | otherwise = n : f (t:ss) n
