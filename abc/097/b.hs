main :: IO ()
main = readLn >>= \x -> print $ maximum [b^p | b<-[1..31], p<-[2..9], b^p <= x]
