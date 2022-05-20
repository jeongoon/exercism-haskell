module Pangram (isPangram) where

import qualified Data.Char as Char
--import qualified Data.Set as Set

isPangram :: String -> Bool
isPangram =
  (26 ==)
  . length
  . (foldr sieve "")

  where
    sieve ch acc =
      let lch = Char.toLower ch
      in
        if isLowerAlhpa26 lch &&
           (not . (ch `elem`) $ acc) then
          lch : acc
        else
          acc

    ord_a = Char.ord 'a'
    ord_z = Char.ord 'z'
    isLowerAlhpa26 ch =

      let och = Char.ord ch
      in
        ord_a <= och && och <= ord_z
