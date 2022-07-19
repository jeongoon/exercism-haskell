module AcronymFoldl2 (abbreviate) where

import Data.Char (isAlpha, isUpper, toUpper)
import Data.List (foldl')

abbreviate :: String -> String
abbreviate =
  (\(racc, _, _) -> reverse racc) . foldl' f ("", False, False)
  where
    f (racc, inWord, lastCharUpper) x
      | isUpper x && not lastCharUpper = (x:racc,           True,                     True)
      | isAlpha x && not inWord =       ((toUpper x):racc, True,                     isUpper x)
      | otherwise    =                  (racc,             (isAlpha x || x == '\''), isUpper x)
