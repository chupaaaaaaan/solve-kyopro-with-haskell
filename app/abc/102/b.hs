import Data.List
main :: IO ()
main = getLine >> sort . map read . words <$> getLine >>= print . uncurry (-) . pair (last, head)
pair (f,g) x = (f x, g x)
