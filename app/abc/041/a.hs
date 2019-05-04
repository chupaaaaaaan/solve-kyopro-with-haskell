main :: IO ()
main=getLine>>= \s->read<$>getLine>>= \i->putStrLn$s!!(i-1):[]
  
