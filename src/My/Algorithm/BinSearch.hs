module My.Algorithm.BinSearch where

import qualified Data.Vector as V

-- lookup the index of the target value from the asc-sorted Vector
bsearchEQ :: Ord a => V.Vector a -> a -> Maybe Int
bsearchEQ vec v = go 0 (V.length vec - 1)
  where go a b | a > b = Nothing
               | otherwise = let m = (a + b) `div` 2
                                 v' = vec V.! m
                             in case v' `compare` v of
                                  EQ -> Just m
                                  GT -> go a (m - 1)
                                  LT -> go (m + 1) b

-- lookup the index of the smallest value greater than the target value from the asc-sorted Vector
bsearchGT :: Ord a => V.Vector a -> a -> Maybe Int
bsearchGT vec v = go lb ub
  where lb = 0
        ub = V.length vec - 1
        go a b | a > ub = Nothing
               | otherwise = let m = (a + b) `div` 2
                                 v' = vec V.! m
                             in case v' `compare` v of
                                  GT | m == lb || (vec V.! (m-1)) <= v -> Just m
                                     | otherwise                       -> go a m
                                  _ -> go (m + 1) b


-- lookup the biggest value lesser than the target value from the asc-sorted Vector
bsearchLT :: Ord a => V.Vector a -> a -> Maybe Int
bsearchLT vec v = go lb ub
  where lb = 0
        ub = V.length vec - 1
        go a b | b < lb = Nothing
               | otherwise = let m = (a + b + 1) `div` 2
                                 v' = vec V.! m
                             in case v' `compare` v of
                                  LT | m == ub || (vec V.! (m+1)) >= v -> Just m
                                     | otherwise                       -> go m b
                                  _ -> go a (m - 1)



-- lookup the index of the smallest value greater equal the target value from the asc-sorted Vector
bsearchGE :: Ord a => V.Vector a -> a -> Maybe (Int, a)
bsearchGE vec v = go lb ub
  where lb = 0
        ub = V.length vec - 1
        go a b | a > ub = Nothing
               | otherwise = let m = (a + b) `div` 2
                                 v' = vec V.! m
                             in case v' `compare` v of
                                  LT -> go (m + 1) b
                                  _ | m == lb || (vec V.! (m-1)) < v -> Just (m, v')
                                    | otherwise                      -> go a m


-- lookup the biggest value lesser than the target value from the asc-sorted Vector
bsearchLE :: Ord a => V.Vector a -> a -> Maybe Int
bsearchLE vec v = go lb ub
  where lb = 0
        ub = V.length vec - 1
        go a b | b < lb = Nothing
               | otherwise = let m = (a + b + 1) `div` 2
                                 v' = vec V.! m
                             in case v' `compare` v of
                                  GT -> go a (m - 1)
                                  _ | m == ub || (vec V.! (m+1)) > v -> Just m
                                    | otherwise                      -> go m b







main :: IO ()
main = do
  x <- readLn :: IO Int
  let vecb = V.fromList   [1,2,3,4,4,4,4,5,5,6,6,6,6,7,8]
  
  putStrLn "====="
  putStrLn "idx: 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14"
  putStrLn "val: 1 2 3 4 4 4 4 5 5 6  6  6  6  7  8"
  putStrLn "====="
  putStrLn $ "LT: " ++ show (bsearchLT vecb x)
  putStrLn $ "LE: " ++ show (bsearchLE vecb x)
  putStrLn $ "EQ: " ++ show (bsearchEQ vecb x)
  putStrLn $ "GE: " ++ show (bsearchGE vecb x)
  putStrLn $ "GT: " ++ show (bsearchGT vecb x)
  putStrLn "====="
