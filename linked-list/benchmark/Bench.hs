{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion
import Criterion.Main (defaultMain)

import qualified IORef as D
import qualified IORefQ as DQ
import qualified IORefA as DTesting

example :: IO ()
example = do
  deque <- D.mkDeque
  D.push deque 'a'
  D.push deque 'b'
  D.push deque 'c'
  D.shift deque
  D.unshift deque 'd'
  D.push deque 'e'
  D.shift deque
  D.pop deque
  D.pop deque
  return ()

exampleQ = do
  deque <- DQ.mkDeque
  DQ.push deque 'a'
  DQ.push deque 'b'
  DQ.push deque 'c'
  DQ.shift deque
  DQ.unshift deque 'd'
  DQ.push deque 'e'
  DQ.shift deque
  DQ.pop deque
  DQ.pop deque
  return ()

example3 = do
  deque <- DTesting.mkDeque
  DTesting.push deque 'a'
  DTesting.push deque 'b'
  DTesting.push deque 'c'
  DTesting.shift deque
  DTesting.unshift deque 'd'
  DTesting.push deque 'e'
  DTesting.shift deque
  DTesting.pop deque
  DTesting.pop deque
  return ()

main :: IO ()
main = do
  defaultMain [
    bgroup "Warming Up"
      [ bench "Example (1)" $ nfIO example3
      ]
    , bgroup "Deque Stable"
      [ bench "Example (1)" $ nfIO example
      , bench "Example (2)" $ nfIO example
      ]
    , bgroup "Deque Testing"
      [ bench "Example (1)" $ nfIO example3
      , bench "Example (2)" $ nfIO example3
      ]
    , bgroup "Deque Qnikst"
      [ bench "Example (1)" $ nfIO exampleQ
      , bench "Example (2)" $ nfIO exampleQ
      ]
    , bgroup "Deque Testing"
      [ bench "Example (1)" $ nfIO example3
      , bench "Example (2)" $ nfIO example3
      ]
     , bgroup "Closing Down"
      [ bench "Example (1)" $ nfIO example
      ]
    ]
