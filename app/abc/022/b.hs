import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- read <$> getLine :: IO Int
  as <- replicateM n (read <$> getLine) :: IO [Int]
  putStrLn $ show (n - (length . group . sort) as)
