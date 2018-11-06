import Data.Char

main :: IO ()
main = do
  (x:xs) <- getLine
  putStrLn $ toUpper x : (map toLower xs)
