import qualified Data.Set as S
main :: IO ()
main = do
  [a,b,k] <- map read . words <$> getLine :: IO [Int]
  putStr . unlines . map show . S.toList . S.fromList $ (take k [a,a+1..b]) ++ (take k [b,b-1..a])
