import Control.Monad
main :: IO ()
main = readLn >>= \n -> sum . map ((\[x,u] -> read x * unit u) . words) <$> replicateM n getLine >>= print

unit :: String -> Double
unit "JPY" = 1.0
unit "BTC" = 380000.0
