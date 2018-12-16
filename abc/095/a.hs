main :: IO ()
main = getLine >>= print . (+700) . (*100) . length . filter (=='o')
