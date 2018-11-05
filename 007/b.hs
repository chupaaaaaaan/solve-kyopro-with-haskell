main :: IO ()
main = do
  x <- getLine
  if x == "a"
    then putStrLn "-1"
    else putStrLn "a"
