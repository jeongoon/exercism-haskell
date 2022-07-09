module YetAnotherAcronym (abbreviate) where

import           Data.Char as C

abbreviate :: String -> String
abbreviate = consHelper
             . foldr reverseCheck (Nothing, "")
  where
    consHelper (mbChar, acc) = case mbChar of
                                 Just ch -> (C.toUpper ch) : acc
                                 Nothing -> acc

    reverseCheck c acc@(mbCandi, abbr)
      | C.isLetter c =
        if camelCase then
          ( Nothing
          , c : abbr ) -- preppend to get right order
        else
          ( Just c -- next candidate
            , abbr )
      | c == '\'' && mbCandi == (Just 's') =
          acc
      | otherwise = 
          ( Nothing
          , consHelper acc )
      where
        camelCase =
          C.isUpper c
          && (C.isUpper <$> mbCandi) == (Just False)
