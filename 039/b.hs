import Data.Maybe
main :: IO ()
main = do
  x <- read <$> getLine :: IO Integer
  let xs = [(y*y*y*y,y) | y<-[1..]]
  print $ fromMaybe 0 $ lookup x xs
