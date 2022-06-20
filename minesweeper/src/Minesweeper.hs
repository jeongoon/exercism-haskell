module Minesweeper (minesweeperRange, countMines, annotate) where

import Data.List (transpose)
import Data.Char (intToDigit)

minesweeperRange :: [a] -> [[a]]
minesweeperRange ms
  | null ms = [[]]
  | otherwise =
      let
        first':rest' = ms
        first:rest = minesweeperRange' $ first':first':rest'
                                       -- ^^^^ dummy: will be removed in the end
      in
        (drop 1 first) : rest
  where
    minesweeperRange' begs -- begs: the group of beginning members
      | null mids = []
      | otherwise =
          concat (map (take 1) [begs, mids, ends])
          : minesweeperRange' mids
      where
        mids = drop 1 begs
        ends = drop 1 mids

countMines :: String -> Int
countMines =
  foldr (\x acc -> if '*' == x then
                     acc+1
                   else
                     acc) 0

annotate :: [String] -> [String]
annotate board =
  let
    makeGroups = transpose . map minesweeperRange
    eachAreaMines :: [[Int]]
    eachAreaMines =
      map (map (countMines . concat))
      . makeGroups
      . makeGroups
      $ board
  in
    zipWith
    (\as bs ->
       zipWith
      (\a b ->
         case (a, b) of
           (_, '*') -> '*'
           (0, _) -> ' '
           (_, _) -> intToDigit a
      )
      as bs)
    eachAreaMines board
