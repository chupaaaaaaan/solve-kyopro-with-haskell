main :: IO ()
main = getLine >> getLine >>= print . sum . map (subtract 1 . read) . words
  
