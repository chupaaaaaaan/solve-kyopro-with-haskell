main :: IO ()
main = do
  _ <- getLine
  s <- getLine
  let w = length . filter (=='.') $ s
  print $ minimum $ map (uncurry (+)) $ solve 0 w s

solve :: Int -> Int -> String -> [(Int,Int)]
solve a b [] = [(a,b)]
solve a b (x:xs)
  | x == '.' = (a,b) : solve a (b-1) xs
  | x == '#' = (a,b) : solve (a+1) b xs
