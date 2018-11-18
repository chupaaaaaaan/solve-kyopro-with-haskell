main :: IO ()
main = do
  a <- getLine
  b <- getLine
  case compare (length a) (length b) of
    GT -> putStrLn "GREATER"
    LT -> putStrLn "LESS"
    EQ -> case compare a b of
            GT -> putStrLn "GREATER"
            LT -> putStrLn "LESS"
            EQ -> putStrLn "EQUAL"
