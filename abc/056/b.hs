main :: IO ()
main = map read . words <$> getLine >>= \(w:a:b:_) -> print $ f' w a b

f w a b
  | a <= b && b < a + w = 0
  | b <= a && a < b + w = 0
  | a + w <= b = b - a - w
  | b + w <= a = a - b - w

f' w a b
  | abs (b - a) < w = 0
  | otherwise = abs (b - a) - w
