{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion
import Criterion.Main (defaultMain)

import qualified Data.Text as T
import qualified Acronym as A
import qualified InProgressAcronym as IA
import qualified YetAnotherAcronym as YA

allInOne = T.concat [ "Portable Network Graphics"
                    , "Ruby on Rails"
                    , "HyperText Markup Language"
                    , "First In, First Out"
                    , "GNU Image Manipulation Program"
                    , "Complementary metal-oxide semiconductor"
                    , "Rolling On The Floor Laughing So Hard That My Dogs Came Over And Licked Me"
                    , "Something - I made up from thin air"
                    , "Halley's Comet"
                    , "The Road _Not_ Taken"
                    ]

allInOne' = T.unpack allInOne

main :: IO ()
main = do
  defaultMain [
      bgroup "By Words"
    [ bench "All in one (1)" $ nf A.abbreviateByWords allInOne
    , bench "All in one (2)" $ nf A.abbreviateByWords allInOne
    ]
    , bgroup "By Boundary"
      [ bench "All in one (1)" $ nf A.abbreviateByBoundaries allInOne
      , bench "All in one (2)" $ nf A.abbreviateByBoundaries allInOne
      ]
    , bgroup "by Boundary With Custom Data Types"
      [ bench "All in one (1)" $ nf IA.abbreviateByBoundaries allInOne
      , bench "All in one (2)" $ nf IA.abbreviateByBoundaries allInOne
      ]
    , bgroup "Foldr Solution"
      [ bench "All in one (1)" $ nf YA.abbreviate allInOne'
      , bench "All in one (2)" $ nf YA.abbreviate allInOne'
      ]
    ]
