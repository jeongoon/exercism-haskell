module Grains (square, total) where

import Data.Maybe (fromJust)
maxSquares :: Integral a => a
maxSquares = 64

square :: Integer -> Maybe Integer
square n
  | n <= 0 = Nothing
  | n > maxSquares = Nothing
  | otherwise = Just $ 2 ^ (pred n)


-- first brute force version by using square function
total_bruteforce :: Integer
total_bruteforce =
  sum
  . fromJust
  . mapM square $ [ 1..64 ]

--  if num of squres(n) is equal or more than 2

-- let's say n is 4

--    1 + 2^1 + 2^2 + 2^3 + 2^4
--  = 1 + (1 +  2^1 + 2^2 + 2^3) * 2
--  = 1 + (1 + (1 +  (2^1 + 2^2) * 2 ) * 2
--  = 1 + (1 + (1 +  (1   + 2^1) * 2 ) * 2 * 2

-- let f(k) = k * 2 + 1

-- we can use iterate and get (k-2) th index(starting from 0) element

total_bruteforce_iterate :: Integer
total_bruteforce_iterate =
  (!! (maxSquares - 2))
  . iterate ((+1) . (*2)) $ 3

-- or we can use foldr
total_bruteforce_foldr :: Integer
total_bruteforce_foldr =
  foldr (\_ acc -> (+1) . (*2) $ acc) 3 [3..maxSquares]


-- or simple calculation
total :: Integer
total = 2 ^ maxSquares -1
