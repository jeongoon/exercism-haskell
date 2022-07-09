{-# LANGUAGE OverloadedStrings #-}

module Acronym (abbreviate, abbreviateByWords, abbreviateByBoundaries) where

import Data.Char (isLetter, isLower, isUpper, toUpper)
import Data.Text (Text)
import qualified Data.Text as T

-- | Convert a phrase to its acronym.
abbreviate :: Text -> Text
abbreviate = abbreviateByWords

-- * Word-based algorithm

-- | Convert a phrase to its acronym. (Word-based algorithm)
abbreviateByWords :: Text -> Text
abbreviateByWords = T.concat . map abbreviateWord . wordsByLetter

-- | Like 'T.words', but include based on 'isLetter' (and @'\''@).
-- >>> wordsByLetter "Halley's Comet"
-- ["Halley's","Comet"]
wordsByLetter :: Text -> [Text]
wordsByLetter = T.words . T.map replace
  where
    replace c | isLetter c = c
    replace '\'' = '\''
    replace _ = ' '

-- | Abbreviate a single word.
-- >>> abbreviateWord <$> ["lowercase","UPPERCASE","camelCase"]
-- ["L","U","CC"]
abbreviateWord :: Text -> Text
abbreviateWord t
  | T.all isUpper t = T.take 1 t
  | Just (c, t') <- T.uncons t = toUpper c `T.cons` T.filter isUpper t'
  | otherwise = t

-- * Boundary-based algorithm

-- | Convert a phrase to its acronym. (Boundary-based algorithm)
abbreviateByBoundaries :: Text -> Text
abbreviateByBoundaries = T.toUpper . filterWithPrev isWordStart ' '

-- | True if a character boundary starts a word.
isWordStart :: Char -> Char -> Bool
isWordStart l r = (word && not apostraphe) || camelCase
  where
    word = not (isLetter l) && isLetter r
    apostraphe = [l, r] == "'s"
    camelCase = isLower l && isUpper r

-- | Like 'T.filter', but also provide the preceding character as context.
filterWithPrev :: (Char -> Char -> Bool) -> Char -> Text -> Text
filterWithPrev keep z = fst . T.foldl' f ("", z)
  where
    f (acc, l) r
      | keep l r = (acc `T.snoc` r, r)
      | otherwise = (acc, r)
