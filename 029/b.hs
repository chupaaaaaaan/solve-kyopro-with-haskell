import Control.Monad
main :: IO ()
main = print =<< length . filter id . map (elem 'r') <$> replicateM 12 getLine
