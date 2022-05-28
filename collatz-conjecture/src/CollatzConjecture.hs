{-# LANGUAGE BangPatterns #-}
module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz = collatz' 0
  where
    collatz' !step k
      | k <= 0 =    Nothing
      | even k =    collatz' step' (k `div` 2)
      | k == 1 =    Just step
      | otherwise = collatz' step' (k*3 + 1)
      where step' = step + 1
