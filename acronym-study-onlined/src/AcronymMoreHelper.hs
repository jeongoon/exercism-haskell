module AcronymMoreHelper (abbreviate) where

import Data.Char (isAlpha, isUpper, toUpper)

abbreviate :: String -> String
abbreviate s = abbreviate' s [] False False
  where
    abbreviate' [] acc _ _ = reverse acc
    abbreviate' (x:xs) acc inWord lastCharUpper
      | camelCase    = abbreviate'' (x:acc)           True True
      | newWord = abbreviate'' ((toUpper x):acc) True newUpper
      | otherwise    = abbreviate'' acc               (isAlpha x || x == '\'') newUpper
      where
        newUpper     = isUpper x
        abbreviate'' = abbreviate' xs -- this is only for shorter expression
                                      -- not for performance
        camelCase    = newUpper && not lastCharUpper
        newWord      = isAlpha x && not inWord
