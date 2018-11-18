import Data.List

main :: IO ()
main = do
  _ <- read <$> getLine :: IO Int
  a <- words <$> getLine
  k <- read <$> getLine :: IO Int
  ps <- nub . words <$> getLine

  if k == length ps && (and . map (flip notElem ps) $ a)
    then putStrLn "YES"
    else putStrLn "NO"
