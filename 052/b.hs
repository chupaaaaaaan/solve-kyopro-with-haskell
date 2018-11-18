main :: IO ()
main = getLine >> getLine >>= print . maximum . scanl (\n c -> if c == 'I' then n + 1 else n - 1) 0

