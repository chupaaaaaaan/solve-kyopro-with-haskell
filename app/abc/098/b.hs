import qualified Data.Set as S
main :: IO ()
main = do
  n <- readLn
  getLine >>= print . maximum .  map (S.size . uncurry S.intersection . dmap S.fromList) . f n

f :: Int -> [a] -> [([a],[a])]
f 0 _ = []
f n xs = splitAt n xs : f (n-1) xs

pair (g,h) x = (g x, h x)
cross (g,h) = pair (g.fst, h.snd)
dmap g = cross (g,g)
