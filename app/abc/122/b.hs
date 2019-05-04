import Data.List
main :: IO ()
main = do
  s <- getLine
  let partS = filter (\x -> x `isInfixOf` s) $ subsequences s 
  print $ maximum $ map length $ filter (\xs -> all (`notElem` xs) "BDEFHIJKLMNOPQRSUVWXYZ") partS


