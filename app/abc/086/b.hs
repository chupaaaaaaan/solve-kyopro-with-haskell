main :: IO ()
main = read.concat.words<$>getLine>>= \n->putStrLn$if(==n).(^2).floor.sqrt.fromIntegral$n then "Yes" else "No"
