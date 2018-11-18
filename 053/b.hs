import Data.List
main :: IO ()
main = getLine >>= print . length . dropWhile (/='A') . dropWhileEnd (/='Z')
