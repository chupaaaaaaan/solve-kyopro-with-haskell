{-# LANGUAGE BangPatterns #-}

module My.Data.IntMod where

import           Data.Int (Int64)

modulus :: Int64
modulus = 1000000007

newtype IntMod = IntMod Int64 deriving Eq

-- fromIntegral_Int64_IntMod :: Int64 -> IntMod
-- fromIntegral_Int64_IntMod n = IntMod (n `mod` modulus)
-- {-# RULES
-- "fromIntegral/Int->IntMod"
--     fromIntegral = fromIntegral_Int64_IntMod . (fromIntegral :: Int -> Int64)
-- "fromIntegral/Int64->IntMod"
--     fromIntegral = fromIntegral_Int64_IntMod
-- #-}

instance Show IntMod where
  show (IntMod x) = show x

instance Num IntMod where
  IntMod x + IntMod y = IntMod ((x + y) `mod` modulus)
  IntMod x - IntMod y = IntMod ((x - y) `mod` modulus)
  IntMod x * IntMod y = IntMod ((x * y) `mod` modulus)
  fromInteger n = IntMod (fromInteger (n `mod` fromIntegral modulus))
  abs = undefined
  signum = undefined

powMod :: IntMod -> Int64 -> IntMod
powMod !x 1 = x
powMod !x !k
  | even k = powMod (x * x) (k `div` 2)
  | otherwise = x * powMod (x * x) (k `div` 2)

invMod :: IntMod -> IntMod
invMod 0  = error "inverse of 0"
invMod !x = powMod x (modulus - 2)

nPkMod :: Int64 -> Int64 -> IntMod
nPkMod !n' !k' = go 1 n' k'
  where go :: IntMod -> Int64 -> Int64 -> IntMod
        go !a !_ 0  = a
        go !a !n !k = go (a * fromIntegral n) (n - 1) (k - 1)

nCkMod :: Int64 -> Int64 -> IntMod
nCkMod !n !k = nPkMod n k * invMod (nPkMod k k)
