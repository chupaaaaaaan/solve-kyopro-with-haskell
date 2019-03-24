main :: IO ()
main = do
  b <- getLine
  putStrLn $ case b of
    "A" -> "T"
    "T" -> "A"
    "C" -> "G"
    "G" -> "C"
    
