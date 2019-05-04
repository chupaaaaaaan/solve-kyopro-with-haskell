import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- (read <$> getLine) :: IO Int
  ss <- replicateM n getLine
  putStrLn . fst . head . sortBy (\(_,a) (_,b) -> compare b a) . map (f (head, length)) . group . sort $ ss

f :: (a -> b, a -> c) -> a -> (b,c)
f (g, h) x = (g x, h x)
