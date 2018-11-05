main :: IO ()
main = do
  s <- getLine
  t <- getLine

  if (and $ map f $ zip s t)
    then putStrLn "You can win"
    else putStrLn "You will lose"
  
f :: (Char, Char) -> Bool
f (a,b)
  | a == b = True
  | a == '@' && elem b "atcoder" = True
  | elem a "atcoder" && b == '@' = True
  | otherwise = False
