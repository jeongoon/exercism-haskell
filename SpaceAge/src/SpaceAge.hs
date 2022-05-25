module SpaceAge (Planet(..), ageOn) where

-- note: this version will not be complied on exercism.org
--       because it depends on Data.Map

import Data.Map (Map, (!))
import qualified Data.Map as Map

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune deriving(Eq, Ord)


secondsPerOneEarthYear :: Int
secondsPerOneEarthYear = 31557600

factorToEarthYearMap =
  Map.fromList
  [ (Mercury,   0.2408467)
  , (Venus,     0.61519)
  , (Earth,     1.0)
  , (Mars,      1.8808158)
  , (Jupiter,   11.862615)
  , (Saturn,    29.447498)
  , (Uranus,    84.016846)
  , (Neptune,   164.79132)
  ]

ageOn :: Planet -> Float -> Float
ageOn planet seconds =
  let
    toEarthYears = (/ (fromIntegral secondsPerOneEarthYear))

  in
    (toEarthYears seconds) / (factorToEarthYearMap ! planet)
