import Data.Array.IArray
import Data.Array.Unboxed

main :: IO ()
main = do
  n <- readLn :: IO Int
  ps <- (0:) . map read . words <$> getLine
  let s = sum ps
      l = length ps - 1
  print $ length $ filter id $ map (\x -> solve2 s l (listArray (0,n) ps) (x, n)) [0..s]
--  print $ map (\x -> solve1 s l (listArray (0,n) ps) (x, n)) [0..s]


solve2 :: Int -> Int -> UArray Int Int  -> (Int,Int) -> Bool
solve2 _ _ _ (y,0) = y == 0
solve2 s l xa (y,n)
  | pt < 0    =                              solve2 s l xa (y, n - 1)
  | otherwise = solve2 s l xa (pt, n - 1) || solve2 s l xa (y, n - 1)
  where pt = y - xa ! n
  

solve1 :: Int -> Int -> UArray Int Int  -> (Int,Int) -> Bool
solve1 _ _ _ (y,0) = y == 0
solve1 s l xa (y,n)
  | pt < 0    =                               memoA s l xa ! (y, n - 1)
  | otherwise = memoA s l xa ! (pt, n - 1) || memoA s l xa ! (y, n - 1)
  where pt = y - xa ! n

memoA :: Int -> Int -> UArray Int Int -> Array (Int,Int) Bool
memoA s l xs = listArray ((0,0),(s,l)) $ map (solve1 s l xs) [(z,m) | z <- [0..s], m <- [0..l]]



-- solve :: [Int] -> Int -> Bool
-- solve xs = go (length xs - 1)
--   where go :: Int -> Int -> Bool
--         go 0 y = y == 0
--         go n y
--           | xs !! n < 0 = go (n - 1) y
--           | otherwise = go (n - 1) (y - xs !! n) || go (n - 1) y




-- I can't make a program with Map!!!
-- type Table = Map (Int,Int) Bool

-- solve1 :: [Int] -> Int -> Bool
-- solve1 xs x = fst $ go (x,length xs - 1) M.empty
--   where go :: (Int,Int) -> Table -> (Bool,Table)
--         go (y,0) tbl = (y == 0,tbl)
--         go (y,m) tbl = case M.lookup (y,m) tbl of
--           Just v -> (v,tbl)
--           Nothing -> (vn,tbln)
--             where pt = xs!!m
--                   go1 = go (y-pt,m-1) tbl
--                   go2 = go (y,m-1) tbl
--                   vn = if y-pt<0
--                        then fst go1 || fst go2
--                        else fst go2
--                   tbln = if y-pt<0
--                          then M.insert (y,m) vn $ M.union (snd go1) (snd go2)
--                          else M.insert (y,m) vn $ snd go2

