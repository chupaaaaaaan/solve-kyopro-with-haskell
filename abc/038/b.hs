main :: IO ()
main = do
  hw1 <- words <$> getLine
  hw2 <- words <$> getLine

  if or . map (flip elem hw2) $ hw1
    then putStrLn "YES"
    else putStrLn "NO"
