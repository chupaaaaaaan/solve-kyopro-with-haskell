import Data.List
main :: IO ()
main = all even . map length . group . sort <$> getLine >>= \x -> if x then putStrLn "Yes" else putStrLn "No"
