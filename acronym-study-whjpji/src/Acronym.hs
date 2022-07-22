module Acronym (abbreviate) where

import Data.Char
import Data.List
import Data.Maybe

abbreviate :: String -> String
abbreviate xs = concat $ map extractHead $ window2 ' ' xs

extractHead :: (Char, Char) -> [Char]
extractHead (a, b)
    | not (isAlpha a || a == '\'') && isAlpha b = [toUpper b]
    | isLower a && isUpper b = [b]
    | otherwise = []

window2 :: Char -> String -> [(Char, Char)]
window2 h xs = zip (h:xs) xs