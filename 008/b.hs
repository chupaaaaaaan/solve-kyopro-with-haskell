import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- (read <$> getLine) :: IO Int
  ss <- replicateM n getLine

