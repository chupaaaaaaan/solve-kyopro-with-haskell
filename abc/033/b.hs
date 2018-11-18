import Control.Monad

main :: IO ()
main = do
  n <- read <$> getLine :: IO Int
  sps <- (\x -> zip (map head x) (map (read.head.tail) x)) <$> replicateM n (words <$> getLine) :: IO [(String,Int)]
  let khns = (sum . map snd $ sps) `div` 2 + 1
  case filter ((>=khns).snd) sps of
    [(name,_)] -> putStrLn name
    _          -> putStrLn "atcoder"


