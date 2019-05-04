main :: IO ()
main = read <$> getLine >>= \n -> read <$> getLine >>= \k -> print . minimum . foldr (<*>) [1] . replicate n $ [(*2),(+k)]
  
