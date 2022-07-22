module AcronymFoldr (abbreviate) where

import           Data.Char as C

abbreviate :: String -> String
abbreviate = consHelper
             . foldr (\c origAcc@(_, rightLower, abbr) ->
                          if C.isAlpha c then
                            let currUpper = C.isUpper c
                            in
                              if currUpper && rightLower then
                                ( Nothing, False,          c:abbr )
                              else
                                ( Just c,  not currUpper, abbr )

                          else if c == '\'' then
                                 origAcc
                               else
                                 ( Nothing
                                 , False
                                 , consHelper origAcc
                                 )
                     )
             (Nothing, True, "")

  where
    consHelper (mbChar, _, acc) = case mbChar of
                                 Just ch -> (C.toUpper ch) : acc
                                 Nothing -> acc
