main :: IO ()
main = do
  s <- reverse <$> getLine
  print $ check s

check :: String -> Int
check s = if (take l d2s) == (drop l d2s) then l * 2 else check d2s
  where l = length s `div` 2 - 1
        d2s = drop 2 s
  
