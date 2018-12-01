import Control.Monad
import Data.List
main :: IO ()
main = readLn >>= flip replicateM getLine >>= print . sum . map product . filter ((==3) . length) . subsequences . map length . groupBy (\x y -> head x == head y) . sort . filter (flip elem "MARCH" . head)
