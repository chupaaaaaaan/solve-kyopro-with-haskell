import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- readLn
  cs <- zip [1..] . map ((\[x,y] -> (x,(read :: String -> Int) y)) .words) <$> replicateM n getLine :: IO [(Int, (String, Int))]
  putStrLn $ unlines $ map (show . fst) $ sortBy (\(_,(y1,z1)) (_,(y2,z2)) -> case compare y1 y2 of
                                                                             LT -> LT
                                                                             GT -> GT
                                                                             EQ -> compare z2 z1) cs


