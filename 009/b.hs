import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- (read <$> getLine) :: IO Int
  as <- replicateM n (read <$> getLine) :: IO [Int]

--  putStrLn . show . head . flip (!!) 1 . group . reverse . sort $ as
  putStrLn . show . flip (!!) 1 . reverse . sort . nub $ as
