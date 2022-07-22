{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion
import Criterion.Main (defaultMain)

import qualified Data.Text as T
import qualified Acronym as A
import qualified Acronym as AR
import qualified AcronymFoldl as AFL
import qualified AcronymFoldr as AFR

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
    bgroup "Warming Up Code"
      [ bench "All in one (1)" $ nf AFR.abbreviate allInOne'
      ]
    , bgroup "Original"
      [ bench "All in one (1)" $ nf A.abbreviate allInOne'
      , bench "All in one (2)" $ nf A.abbreviate allInOne'
      ]
    , bgroup "Revised"
      [ bench "All in one (1)" $ nf AR.abbreviate allInOne'
      , bench "All in one (2)" $ nf AR.abbreviate allInOne'
      ]
    , bgroup "Foldl"
      [ bench "All in one (1)" $ nf AFL.abbreviate allInOne'
      , bench "All in one (2)" $ nf AFL.abbreviate allInOne'
      ]
    , bgroup "Foldr"
      [ bench "All in one (1)" $ nf AFR.abbreviate allInOne'
      , bench "All in one (2)" $ nf AFR.abbreviate allInOne'
      ]
    , bgroup "Closing Code"
      [ bench "All in one (1)" $ nf AFL.abbreviate allInOne'
      ]
    ]
