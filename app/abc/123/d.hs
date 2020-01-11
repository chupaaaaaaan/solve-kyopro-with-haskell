import Data.List

main :: IO ()
main = do
  [_,_,_,k] <- map read . words <$> getLine :: IO [Int]
  as <- sortBy (flip compare) . map read . words <$> getLine :: IO [Integer]
  bs <- sortBy (flip compare) . map read . words <$> getLine :: IO [Integer]
  cs <- sortBy (flip compare) . map read . words <$> getLine :: IO [Integer]
  let asd = diffn as
      bsd = diffn bs
      csd = diffn cs

  print $ take k $ solve as bs cs asd bsd csd
  print as
  print bs
  print cs
  print asd
  print bsd
  print csd

inf :: Integer
inf = 10^11

diffn :: [Integer] -> [Integer]
diffn [] = []
diffn [_] = [inf]
diffn (x:y:xs) = (x-y) : diffn (y:xs)

solve :: [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer] -> [Integer]
solve _ _ _ [] _ _ = []
solve _ _ _ _ [] _ = []
solve _ _ _ _ _ [] = []
solve (a':as') (b':bs') (c':cs') (ad':asd') (bd':bsd') (cd':csd')
  | m == ad' = (a'+b'+c') : solve as' (b':bs') (c':cs') asd' (bd':bsd') (cd':csd')
  | m == bd' = (a'+b'+c') : solve (a':as') bs' (c':cs') (ad':asd') bsd' (cd':csd')
  | m == cd' = (a'+b'+c') : solve (a':as') (b':bs') cs' (ad':asd') (bd':bsd') csd'
  where m = minimum [ad',bd',cd']
