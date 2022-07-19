module Acronym (abbreviate) where

import Data.Char (isAlpha, isUpper, toUpper)

abbreviate :: String -> String
abbreviate s = abbreviate' s [] False False
  where
    abbreviate' [] acc _ _ = reverse acc
    abbreviate' (x:xs) acc inWord lastCharUpper
      | (isUpper x && not lastCharUpper) || (isAlpha x && not inWord) = abbreviate' xs ((toUpper x):acc) True (isUpper x)
      | otherwise = abbreviate' xs acc (isAlpha x || x == '\'') (isUpper x) 
      

