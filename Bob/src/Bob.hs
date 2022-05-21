module Bob (responseFor) where

import qualified Data.Char as Char

sure, whatever, chillout, calmdown, beThatWay :: String
sure      = "Sure."
whatever  = "Whatever."
chillout  = "Whoa, chill out!"
calmdown  = "Calm down, I know what I'm doing!"
beThatWay = "Fine. Be that way!"

responseFor :: String -> String
responseFor givenStr
  | null str = beThatWay
  | areAllAlphabetsUpper =
      if unsafe_isQuestion then calmdown
      else chillout
  | unsafe_isQuestion = sure
  | otherwise = whatever

  where
    str = filter (not . Char.isSpace) givenStr
    unsafe_isQuestion = -- don't check givenStr is null or has any spaces
      last str == '?'
    alphabetsOnly = filter Char.isAlpha str
    areAllAlphabetsUpper = all (\f -> f alphabetsOnly)
      [ not . null
      , all Char.isUpper
      ]
