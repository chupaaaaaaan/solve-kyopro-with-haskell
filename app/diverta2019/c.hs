import Data.List
import Control.Monad

main :: IO ()
main = do
  n <- readLn :: IO Int
  ss <- replicateM n getLine
  let baseab = length . filter (=="AB") . concatMap pair $ ss
      lasta  = length . filter (not . isPrefixOf "B") . filter (isSuffixOf "A") $ ss
      firstb = length . filter (not . isSuffixOf "A") . filter (isPrefixOf "B") $ ss
      bothab = length . filter (      isSuffixOf "A") . filter (isPrefixOf "B") $ ss
  print $ baseab + if lasta == 0 && firstb == 0 && bothab > 0
                   then bothab - 1
                   else bothab + min lasta firstb

pair :: [a] -> [[a]]
pair [] = []
pair [x] = []
pair (x:y:xs) = [x,y] : pair (y:xs)
