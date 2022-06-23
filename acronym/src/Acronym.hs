module Acronym (abbreviate) where

import           Data.Char as C

abbreviate :: String -> String
abbreviate = (uncurry consHelper)
             . foldr (\c (mbCandi, abbr) ->
                          if C.isAlpha c then
                            if (not . C.isUpper $ c)
                               || (C.isUpper <$> mbCandi) /= (Just False) then
                                ( Just c, abbr )
                            else
                                ( Nothing, c : abbr )

                          else if c == '\'' then
                                 ( mbCandi, abbr )
                               else
                                 ( Nothing
                                 , consHelper mbCandi abbr )
                       )
             (Nothing, "")
  where
    consHelper mbChar acc = case mbChar of
                              Just ch -> (C.toUpper ch) : acc
                              Nothing -> acc
