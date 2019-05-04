-- solved by dp (by Array)
{-# LANGUAGE RankNTypes #-}
import Control.Monad
import Control.Monad.ST
import Data.Array.ST

type Idx = (Int,Int,Int)

main :: IO ()
main = do
  [n,a] <- map read . words <$> getLine :: IO [Int]
  xs <- (0:) . map read . words <$> getLine :: IO [Int]
  print $ sum $ solve n a xs

solve :: Int -> Int -> [Int] -> [Int]
solve m b ys = runST $ do
  let smax = m * maximum (b:ys)
  dptbl <- newArray ((0,0,0),(m,m,smax)) (-1) :: forall s. ST s (STUArray s Idx Int)
  forM_ [(jt,k,s) | s <- [0..smax], k <- [0..m], jt <- zip [0..] ys] $ \((j,yj),k,s) -> do
    ra <- readArray dptbl (j,k,s) 
    case ra of
      -1 | (j,k,s) == (0,0,0)    -> writeArray dptbl (j,k,s) 1
         | j>=1 && s<yj          -> readArray dptbl (j-1,k,s) >>= writeArray dptbl (j,k,s)
         | j>=1 && k>=1 && s>=yj -> liftM2 (+) (readArray dptbl (j-1,k,s)) (readArray dptbl (j-1,k-1,s-yj)) >>= writeArray dptbl (j,k,s)
         | otherwise             -> writeArray dptbl (j,k,s) 0
      _                          -> return ()
  mapM (\k -> readArray dptbl (m,k,k*b)) [1..m]


-- solved by dp (by Map)
-- TLE solution.
-- import Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as M

-- main :: IO ()
-- main = do
--   [n,a] <- map read . words <$> getLine :: IO [Int]
--   xs <- (0:) . map (subtract a . read) . words <$> getLine :: IO [Int]
--   let smax = n * maximum (a:xs)
--   print $ (\x -> fst x - 1) $ solve n smax xs (n,0) M.empty

-- type Table = Map (Int,Int) Integer

-- solve :: Int -> Int -> [Int] -> (Int,Int) -> Table -> (Integer, Table)
-- solve _ _ _ (0,0) dptbl = (1,dptbl)
-- solve m sm ys (j,s) dptbl = case M.lookup (j,s) dptbl of
--   Just x -> (x,dptbl)
--   Nothing -> (xn,dptbln)
--   where
--     yj = ys !! j
--     dp1 = solve m sm ys (j-1,s)    dptbl
--     dp2 = solve m sm ys (j-1,s-yj) dptbl
--     xn
--       | j>=1 && ((s-yj)<(-m*sm) || (s-yj)>(m*sm))   = fst dp1
--       | j>=1 && ((s-yj)>=(-m*sm) && (s-yj)<=(m*sm)) = fst dp1 + fst dp2
--       | otherwise                                   = 0
--     dptbln
--       | j>=1 && ((s-yj)<(-m*sm) || (s-yj)>(m*sm))   = M.insert (j,s) xn $ snd dp1
--       | j>=1 && ((s-yj)>=(-m*sm) && (s-yj)<=(m*sm)) = M.insert (j,s) xn $ M.union (snd dp1) (snd dp2)
--       | otherwise                                   = dptbl





-- solved by brute-force search
-- import Data.List
-- main :: IO ()
-- main = do
--   [n,a] <- map read . words <$> getLine :: IO [Int]
--   xs <- map read . words <$> getLine :: IO [Int]
--   print . length . filter (==Just a) . map average . subsequences $ xs
  
-- average :: [Int] -> Maybe Int
-- average [] = Nothing
-- average xs = let s = sum xs
--                  l = length xs
--              in if s`mod`l==0
--                 then Just $ s`div`l
--                 else Nothing
