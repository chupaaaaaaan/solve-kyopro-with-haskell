import qualified Data.Set as S

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- map read . words <$> getLine :: IO [Integer]
  putStrLn $ if S.size (S.fromList as) == n then "YES" else "NO"
