import Control.Monad
import Data.List
import Data.Maybe

main :: IO ()
main = do
  n <- readLn
  ss <- map (\x -> (head x,length x)) . group . sort <$> replicateM n getLine
  m <- readLn
  ts <- map (\x -> (head x,length x)) . group . sort <$> replicateM m getLine
  print $ maximum . map (\(x,y) -> max 0 (y - (fromMaybe 0 . lookup x) ts)) $ ss
