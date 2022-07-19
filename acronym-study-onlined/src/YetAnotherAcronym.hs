module YetAnotherAcronym (abbreviate) where

import           Data.Char as C

abbreviate :: String -> String
abbreviate = consHelper
             . foldr reverseCheck ("", False, "")
  where
    consHelper (mbChar, _, acc) = case mbChar of
                                    Just ch -> (C.toUpper ch) : acc
                                    Nothing -> acc

    reverseCheck c acc@(_, wasUpper, abbr)
      | C.isUpper c && not wasUpper = ( Nothing, True, c : abbr )
      | C.isLetter c =  ( Just c, False, abbr )
      | c == '\'' = acc
      | otherwise = ( Nothing, False, consHelper acc )
