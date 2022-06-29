module Acronym (abbreviate) where

import           Data.Char as C

abbreviate :: String -> String
abbreviate = consHelper
             . foldr (\c origAcc@(mbCandi, abbr) ->
                          if C.isAlpha c then
                            if C.isUpper c
                               && (C.isUpper <$> mbCandi) == (Just False) then
                                ( Nothing, c : abbr )
                            else
                                ( Just c, abbr )

                          else if c == '\'' then
                                 ( mbCandi, abbr )
                               else
                                 ( Nothing
                                 , consHelper origAcc
                                 )
                     )
             (Nothing, "")
  where
    consHelper (mbChar, acc) = case mbChar of
                                 Just ch -> (C.toUpper ch) : acc
                                 Nothing -> acc
