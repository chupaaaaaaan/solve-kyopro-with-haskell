import Data.List
main :: IO ()
main = do
  getLine
  as <- map read . words <$> getLine :: IO [Integer]
  print . length . last . group . sort . f $ as

f :: Integral a => [a] -> [a]
f [] = []
f (_:[]) = []
f (a:as) = (maximum (map (subtract a) as)) : f as
