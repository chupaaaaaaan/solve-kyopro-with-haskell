import Data.List
main :: IO ()
main = getLine >> reverse . sort . map read . words <$> getLine >>= print . (\(x,y) -> x - y) . f (0,0)

f :: (Int,Int) -> [Int] -> (Int,Int)
f (a,b) [] = (a,b)
f (a,b) [x] = (a+x,b)
f (a,b) (x:y:xs) = f (a+x,b+y) xs
