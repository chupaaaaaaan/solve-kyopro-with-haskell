import qualified Data.Set as Set

main :: IO ()
main = do
  s <- getLine
  k <- read <$> getLine :: IO Int
  print $ Set.size $ Set.fromList $ f k s

f :: Int -> [a] -> [[a]]
f k xs
  | k <= length xs = (take k xs) : f k (tail xs)
  | otherwise = []

