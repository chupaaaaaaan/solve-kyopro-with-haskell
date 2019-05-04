module My.Data.Comparable where

import Data.Proxy

data Lesser
data Greater

class Comparable tag where
  comp :: Ord a => Proxy tag -> a -> a -> Bool

instance Comparable Lesser where
  comp _ = (<=)

instance Comparable Greater where
  comp _ = (>=)
