module Isogram (isIsogram) where

import qualified Data.Char as C
import Control.Arrow ((***))

-- This solution is only for fun
-- function composition in haskell is intriging!

isIsogram :: String -> Bool
isIsogram =
  isIsogram'
  . foldr (\c acc ->
            if C.isAlpha c then
              C.toLower c : acc
            else
              acc) ""

  where
    isFirstMemberUnique =
      head >>= (curry $ (not . uncurry elem) . (id *** tail))

    isIsogram' =
      null >>=
      (\cond ->
         if cond then const True
         else
           (&&) <$> isFirstMemberUnique <*> (isIsogram . tail)
      )
