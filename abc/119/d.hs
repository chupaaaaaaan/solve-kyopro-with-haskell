import Control.Monad
import Data.Maybe

import qualified Data.Vector as V

-- lookup the index of the target value from the asc-sorted Vector
bsearchEQ :: Ord a => a -> V.Vector a -> Maybe Int
bsearchEQ v vec = go 0 (V.length vec - 1)
  where go a b | a > b = Nothing
               | otherwise = let m = (a + b) `div` 2
                                 v' = (vec V.! m)
                             in case v' `compare` v of
                                  EQ -> Just m
                                  GT -> go a (m - 1)
                                  LT -> go (m + 1) b

-- lookup the index of the smallest value greater than the target value from the asc-sorted Vector
bsearchGT :: Ord a => a -> V.Vector a -> Maybe Int
bsearchGT v vec = go lb ub
  where lb = 0
        ub = (V.length vec - 1)
        go a b | a > ub = Nothing
               | otherwise = let m = (a + b) `div` 2
                                 v' = (vec V.! m)
                             in case v' `compare` v of
                                  GT | m == lb || (vec V.! (m-1)) <= v -> Just m
                                     | otherwise                       -> go a m
                                  _ -> go (m + 1) b


-- lookup the biggest value lesser than the target value from the asc-sorted Vector
bsearchLT :: Ord a => a -> V.Vector a -> Maybe Int
bsearchLT v vec = go lb ub
  where lb = 0
        ub = (V.length vec - 1)
        go a b | b < lb = Nothing
               | otherwise = let m = (a + b + 1) `div` 2
                                 v' = (vec V.! m)
                             in case v' `compare` v of
                                  LT | m == ub || (vec V.! (m+1)) >= v -> Just m
                                     | otherwise                       -> go m b
                                  _ -> go a (m - 1)



-- lookup the index of the smallest value greater equal the target value from the asc-sorted Vector
bsearchGE :: Ord a => a -> V.Vector a -> Maybe Int
bsearchGE v vec = go lb ub
  where lb = 0
        ub = (V.length vec - 1)
        go a b | a > ub = Nothing
               | otherwise = let m = (a + b) `div` 2
                                 v' = (vec V.! m)
                             in case v' `compare` v of
                                  LT -> go (m + 1) b
                                  _ | m == lb || (vec V.! (m-1)) < v -> Just m
                                    | otherwise                      -> go a m


-- lookup the biggest value lesser than the target value from the asc-sorted Vector
bsearchLE :: Ord a => a -> V.Vector a -> Maybe Int
bsearchLE v vec = go lb ub
  where lb = 0
        ub = (V.length vec - 1)
        go a b | b < lb = Nothing
               | otherwise = let m = (a + b + 1) `div` 2
                                 v' = (vec V.! m)
                             in case v' `compare` v of
                                  GT -> go a (m - 1)
                                  _ | m == ub || (vec V.! (m+1)) > v -> Just m
                                    | otherwise                      -> go m b

main :: IO ()
main = do
  [a,b,q] <- map read . words <$> getLine

  s <- V.fromList . map read <$> replicateM a getLine :: IO 
  t <- V.fromList . map read <$> replicateM b getLine :: IO 
  xs <- replicateM q readLn :: IO [Int]
  

  let boundAdd = \x ve -> V.cons (negate x) (V.snoc ve x)
      ss = boundAdd (10^13) s
      ts = boundAdd (10^13) t
      sis = map (\x -> fromJust $ bsearchGE x ss) xs
      tis = map (\x -> fromJust $ bsearchGE x ts) xs
      tmi = map (\x -> fromJust $ bsearchGE x ts) xs


  -- mapM_ print $ zipWith5 (\sl tl x sg tg -> minimum
  --                                           [x-sl+abs(sl-tl)
  --                                           ,x-sl+abs(sl-tg)
  --                                           ,sg-x+abs(sg-tl)
  --                                           ,sg-x+abs(sg-tg)
  --                                           ,x-tl+abs(tl-sl)
  --                                           ,x-tl+abs(tl-sg)
  --                                           ,tg-x+abs(tg-sl)
  --                                           ,tg-x+abs(tg-sg)]) sls tls xs sgs tgs
