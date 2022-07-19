{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion
import Criterion.Main (defaultMain)

import qualified Data.Text as T
import qualified Acronym as A
import qualified AcronymFewHelper as AF
import qualified AcronymMoreHelper as AM
import qualified AcronymFoldl as AFL
import qualified AcronymFoldl as AFL2

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
      [ bench "All in one (1)" $ nf AF.abbreviate allInOne'
      ]
    , bgroup "Original"
      [ bench "All in one (1)" $ nf A.abbreviate allInOne'
      , bench "All in one (2)" $ nf A.abbreviate allInOne'
      ]
    , bgroup "Few Helper"
      [ bench "All in one (1)" $ nf AF.abbreviate allInOne'
      , bench "All in one (2)" $ nf AF.abbreviate allInOne'
      ]
    , bgroup "More Helper"
      [ bench "All in one (1)" $ nf AM.abbreviate allInOne'
      , bench "All in one (2)" $ nf AM.abbreviate allInOne'
      ]
    , bgroup "Foldl"
      [ bench "All in one (1)" $ nf AFL.abbreviate allInOne'
      , bench "All in one (2)" $ nf AFL.abbreviate allInOne'
      ]
    , bgroup "Closing Code"
      [ bench "All in one (1)" $ nf A.abbreviate allInOne'
      ]
    ]
