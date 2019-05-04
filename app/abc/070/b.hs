main :: IO ()
main = map read . words <$> getLine >>= print . f

f :: [Int] -> Int
f [a,b,c,d]
  | a <= c && b < c = 0
  | a <= c && b >= c && b <= d = b - c
  | a <= c && b > d = d - c
  | c <= a && d < a = 0
  | c <= a && d >= a && d <= b = d - a
  | c <= a && d > b = b - a
