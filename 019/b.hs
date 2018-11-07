import Data.List
main :: IO ()
main = putStrLn =<< filter (/=' ') . unwords . map f . group <$> getLine

f :: String -> String
f a = head a : (show $ length a)
