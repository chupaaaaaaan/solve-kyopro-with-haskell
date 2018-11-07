import Control.Monad
import qualified Data.Set as Set

main :: IO ()
main = do
  n <- read <$> getLine :: IO Int
  as <- Set.fromList <$> replicateM n (read <$> getLine) :: IO (Set.Set Int)
  putStrLn $ show  (n - Set.size as)
