-- | 

module Day00 where

test :: Bool
test =
  fuel 12 == 2 &&
  fuel 14 == 2 &&
  fuel 1969 == 654 &&
  fuel 100756 == 33583

fuel :: Int -> Int
fuel = max 0 . subtract 2 . flip div 3
