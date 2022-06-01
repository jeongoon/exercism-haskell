module SumOfMultiples (sumOfMultiples) where

import Data.List (sort)

unionSort :: Integral a => [a] -> [a] -> [a]
unionSort [] ys = ys
unionSort xs [] = xs
unionSort xs@(x:xt) ys@(y:yt) =
  case x `compare` y of
    LT -> x : unionSort xt ys
    EQ -> x : unionSort xt yt
    GT -> y : unionSort xs yt

foldt :: Integral a => [[a]] -> [a]
foldt [] = []
foldt ([]:_) = [] -- foldt get sorted list only : if left empty: so will right.
foldt ((x:xs):t) = x : unionSort xs (foldt (pairs t))

pairs :: Integral a => [[a]] -> [[a]]
-- edge cases ...
pairs [] = []
pairs ([]:_) = [] -- left always has longer list; no need to go further
pairs (ms:[]) = [ms]

pairs ((m:ms):ns:t') = (m : unionSort ms ns) : pairs t'

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
  sum $ foldt [ [n,n+n..(limit -1)] | n <- factors', n > 0 ]
  where factors' = sort factors
