import Data.List

main :: IO ()
main = do
  n <- read <$> getLine :: IO Int
  as <- map read . words <$> getLine :: IO [Int]

  let avg = sum as
  print . snd . head . sort . mapfst (abs . subtract avg . (*n)) $ zip as [0..]

mapfst :: (a -> b) -> [(a,c)] -> [(b,c)]
mapfst _ [] = []
mapfst f ((x,y):xys) = (f x,y):mapfst f xys
