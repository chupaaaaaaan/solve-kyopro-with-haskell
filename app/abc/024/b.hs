import Control.Monad

main :: IO ()
main = do
  [n',t'] <- words <$> getLine
  let n = read n' :: Int
      t = read t' :: Integer
  as <- map read <$> replicateM n getLine :: IO [Integer]

  putStrLn (show (t + (sum  (f t as))))


f :: Integer ->  [Integer] -> [Integer]
f _ [] = []
f _ (_:[]) = []
f t (x:y:xs)
  | y - x > t = t : f t (y:xs)
  | otherwise = (y - x) : f t (y:xs)
