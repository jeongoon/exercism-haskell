{-# LANGUAGE OverloadedStrings #-}

module InProgressAcronym (abbreviate, abbreviateByWords, abbreviateByBoundaries) where

import Data.Char (isLetter, isLower, isUpper, toUpper)
import Data.Text (Text)
import qualified Data.Text as T

-- | Convert a phrase to its acronym.
abbreviate :: Text -> Text
abbreviate = abbreviateByBoundaries

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

data BoundaryStatus a
  = Candidate a
  | InWordStartWith a
  | OnApostraphe
  | OnBoundary
  deriving (Eq)

-- | Convert a phrase to its acronym. (Boundary-based algorithm)
abbreviateByBoundaries :: Text -> Text
abbreviateByBoundaries = filterWithPrev isWordStart OnBoundary

-- | True if a character boundary starts a word.
isWordStart :: BoundaryStatus Char -> Char -> BoundaryStatus Char
isWordStart st c =
  case st of
    OnBoundary
      | isLetter c -> InWordStartWith c
      | otherwise -> Candidate c

    OnApostraphe ->
      if c == 's' then
        OnBoundary
      else
        isWordStart OnBoundary c -- retry

    Candidate _
      | isLetter c ->
        if isUpper c then
          InWordStartWith c
        else
          Candidate c
      | c == '\'' -> OnApostraphe
      | otherwise -> OnBoundary

    InWordStartWith p ->
      if isLetter c then
        if isLower c then
          Candidate c
        else
          InWordStartWith p
      else
        OnBoundary

-- | Like 'T.filter', but also provide the preceding character as context.
filterWithPrev :: (BoundaryStatus Char -> Char -> BoundaryStatus Char) ->
                  (BoundaryStatus Char) -> Text -> Text
filterWithPrev keep st = fst . T.foldl' f ("", st)
  where
    f (acc, prevStat) c
      | stat == (InWordStartWith c)
        && prevStat /= stat = (acc `T.snoc` (toUpper c), stat)
      | otherwise = (acc, stat)
      where
        stat = keep prevStat c
