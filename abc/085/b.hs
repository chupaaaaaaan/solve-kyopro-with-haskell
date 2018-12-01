import Data.List
import Control.Monad
main :: IO ()
main = readLn >>= \n -> replicateM n getLine >>= print . length . group . sort
