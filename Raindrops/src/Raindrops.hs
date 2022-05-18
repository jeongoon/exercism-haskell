{-# LANGUAGE OverloadedStrings #-}

module Raindrops (convert)  where

import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Maybe as MB

convert :: Int -> Text
convert n =
  MB.fromMaybe (T.pack . show $ n) . (foldr helper Nothing) $
  ([ (3, "Pling")
   , (5, "Plang")
   , (7, "Plong")
   ] :: [ (Int, Text) ])

  where
    helper (k, sound) acc 
      | n `rem` k == 0 =
          case acc of
            Nothing ->
              Just sound
            _ ->
              (sound <>) <$> acc
      | otherwise = acc
