import Data.Char
main :: IO ()
main = do
  s <- getLine
  let a = (=='A') (head s)
      b = (==1) $ length . filter (=='C') $ init . drop 2 $ s
      c = all isLower . filter (/='C') . tail $ s
  putStrLn $ if a && b && c then "AC" else "WA"
