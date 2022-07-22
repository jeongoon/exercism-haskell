module AcronymFoldl (abbreviate) where

import Data.Char (isAlpha, isUpper, toUpper)
import Data.List (foldl')

abbreviate :: String -> String
abbreviate =
  (\(racc, _, _) -> reverse racc) . foldl' f ("", False, False)
  where
    f (racc, inWord, lastCharUpper) x
      | camelCase = (x:racc,           True,                     True)
      | newWord   = ((toUpper x):racc, True,                     newUpper)
      | otherwise = (racc,             (isAlpha x || x == '\''), newUpper)
      where
        newUpper  = isUpper x
        camelCase = newUpper && not lastCharUpper
        newWord   = isAlpha x && not inWord
