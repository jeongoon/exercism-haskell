module Luhn (isValid) where

import qualified Data.Char as C
import Control.Monad (foldM)

isValid :: String -> Bool
isValid =
  maybe False
  (\(cnt, cnt9, sum1, sum2)->
     cnt > 1 && ((sum1 + sum2 + sum2 - 9 * cnt9) `rem` 10 == 0)
  )
  . foldM
  ( \orig@(cnt, cnt9, sum1, sum2) c ->
      if C.isNumber c then
        let d = toInteger $ (fromEnum c) - zero
            addCnt9 = toInteger . fromEnum $ d > 4
        in
          pure $
          if cnt `rem` 2 == 0 then
            (cnt + 1, cnt9,           sum1 + d, sum2)
          else
            (cnt + 1, cnt9 + addCnt9, sum1,     sum2 + d)

      else
        if c == ' ' then
          pure orig
        else
          fail "only digit and space is allowed!"

  ) (0 :: Integer, 0 :: Integer, 0, 0)
  . reverse
  where
    zero = fromEnum '0'
