import Control.Monad
main :: IO ()
main = do
  s <- getLine
  n <- (read <$> getLine) :: IO Int
  rls <- replicateM n (map read . words <$> getLine) :: IO [[Int]]
  putStrLn . foldl perform s $ rls

perform :: String -> [Int] -> String
perform s [l,r] = take (l-1) s ++ (reverse . drop (l-1) . take r $ s) ++ drop r s
