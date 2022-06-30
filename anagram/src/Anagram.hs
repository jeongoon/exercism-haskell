module Anagram (anagramsFor) where

import qualified Data.Char as Char
import qualified Data.List as List

anagramsFor :: String -> [String] -> [String]
anagramsFor str =
  filter
  (\cand ->
     let cand' = map Char.toLower cand
     in
       List.sort cand' == List.sort str' && cand' /= str'
  )
  where
    str' = map Char.toLower $ str
