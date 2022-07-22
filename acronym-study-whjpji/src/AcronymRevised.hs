module AcronymRevised (abbreviate) where

import Data.Char

abbreviate :: String -> String
abbreviate =
  let
    accumulate racc _ _ [] = reverse racc
    -- note : racc means reversed accumulators
    --        so we will reverse it when we finish accumulating
    -- important:
    -- (newElement : acc) is much faster in haskell, when compared to
    -- acc ++ [newElement]

    accumulate racc inTheMiddleOfWord lastCharLower (w:ws) =
      let (inTheMiddleOfWord', currCharLower, mbHead) =
            extractHead' (inTheMiddleOfWord, lastCharLower) w
      in
        accumulate
        -- start new interation with ...
        ( case mbHead of
            Just ch -> (ch : racc)
                       --- ^ add to the accumulator
            Nothing -> racc
                       --- ^ skip
        )
        inTheMiddleOfWord' currCharLower ws
  in
    accumulate [] False True -- initial state
                   --- ^ it wasn't in the middle of word
                          --- ^ and we can say last char was lower case
                          --    because if check first character it is always
                          --    a character for abbr.


extractHead' :: (Bool, Bool) -> Char -> (Bool, Bool, Maybe Char)
-- note: function type has been changed
extractHead' (inTheMiddleOfWord, lastCharLower) c
    -- condition 1
    | lastCharLower && currCharUpper =
        ( True -- still in the middle of a word
        , False -- as currCharUpper is True
        , Just c
        )

    -- condition 2
    -- note: from condition 1, we can know that
    --                         last char is upper or current char is lower
    | not inTheMiddleOfWord && currCharAlpha = -- which means new word
        ( True -- new word has just begun
        , currCharLower
        , Just (toUpper c)
        )

    | otherwise = -- in the middle word or current char is not an alphabet
        ( currCharAlpha || c == '\''
        , currCharLower
        , Nothing
        )

  where
    currCharAlpha = isAlpha c
    currCharUpper = isUpper c
    currCharLower' = currCharAlpha && not currCharUpper
    currCharLower = {-currCharAlpha &&-} not currCharUpper
                    -------------------- ^ we can skip because `condition 2`
                    -- already checked currCharAlpha,
                    -- and when `otherwise`:
                    -- curr char is not alphabet (from condition 2)
                    -- or curr char is upper (from condition 1)

{- -- we don't use at the momment
window2 :: Char -> String -> [(Char, Char)]
window2 h xs = zip (h:xs) xs
-}
