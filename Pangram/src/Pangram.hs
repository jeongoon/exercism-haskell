module Pangram (isPangram) where

import qualified Data.Char as Char
import qualified Data.Set as Set
import Data.List (nub)

isPangram = isPangramCustom

isPangramSet :: String -> Bool
isPangramSet =
  (26==)
  . Set.size
  . foldr sieve Set.empty

  where
    isLowerAlhpa26 lch = -- lch : lower character
        'a' <= lch && lch <= 'z'

    sieve c acc =
      let lc = Char.toLower c
      in
        if isLowerAlhpa26 lc then
          (Set.insert lc acc)
        else
          acc

isPangramCustom :: String -> Bool
isPangramCustom =
  (26 ==)
  . length
  . take 26
  . accPangram' ""

  where
    accPangram' acc [] = []
    accPangram' acc (c:cs) =
      let lc = Char.toLower c
      in
        if isLowerAlhpa26 lc
           && (not . elem lc $ acc) then
          True : accPangram' (lc:acc) cs
        else
          accPangram' acc cs

    isLowerAlhpa26 lch =
        'a' <= lch && lch <= 'z'
