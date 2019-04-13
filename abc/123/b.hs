import Control.Monad
import Data.List

main :: IO ()
main = do
  as <- replicateM 5 readLn :: IO [Int]
  let dms = map (\x -> ((x`div`10)*10,(x-1)`mod`10)) as
      sortedDms = sortBy (\(_,x) (_,y) -> compare x y) dms
      (s:ss) = map (\(x,y) -> (x,(y+1)`mod`10)) sortedDms
      ss2 = map (\(x,y) -> (if y==0 then x else x+10)) ss
  print $ fst s + snd s + sum ss2
