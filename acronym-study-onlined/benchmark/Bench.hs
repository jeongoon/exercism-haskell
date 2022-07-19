{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion
import Criterion.Main (defaultMain)

import qualified Data.Text as T
import qualified Acronym as A
import qualified AcronymSalkyus as AS
import qualified InProgressAcronym as IA
import qualified YetAnotherAcronym as YA
import qualified Onlined as O
import qualified OnlinedRevised as OR
import qualified OnlinedRevised as OR2

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
    bgroup "Onlined Revised"
      [ bench "All in one (1)" $ nf OR.abbreviate allInOne'
      ]
    , bgroup "Onlined Revised 2"
      [ bench "All in one (1)" $ nf OR2.abbreviate allInOne'
      , bench "All in one (2)" $ nf OR2.abbreviate allInOne'
      ]
    , bgroup "Onlined Revised"
      [ bench "All in one (1)" $ nf OR.abbreviate allInOne'
      , bench "All in one (2)" $ nf OR.abbreviate allInOne'
      ]
    , bgroup "Onlined Revised"
      [ bench "All in one (1)" $ nf OR.abbreviate allInOne'
      ]
    ]
