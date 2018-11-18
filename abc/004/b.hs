import Data.List

main :: IO ()
main = do
  c0 <- filter (/=' ') <$> getLine
  c1 <- filter (/=' ') <$> getLine
  c2 <- filter (/=' ') <$> getLine
  c3 <- filter (/=' ') <$> getLine

  putStrLn $ intersperse ' ' $ reverse c3
  putStrLn $ intersperse ' ' $ reverse c2
  putStrLn $ intersperse ' ' $ reverse c1
  putStrLn $ intersperse ' ' $ reverse c0
