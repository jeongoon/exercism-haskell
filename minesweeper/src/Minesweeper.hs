module Minesweeper where

import Data.List (transpose)
import Data.Char (intToDigit)

mineWindow :: [a] -> [[a]]
mineWindow [] = [[]]
mineWindow (first':rest') =
  let
    first:rest = mineWindow' $ first':first':rest'
                                     -- ^^^^ dummy: will be removed in the end
  in
    (drop 1 first) : rest
  where
    mineWindow' (beg : mids@(mid : ends)) =
      (if null ends then  [ beg, mid ]
       else               [ beg, mid, head ends ]) : mineWindow' mids
    mineWindow' _ = []

countMines :: String -> Int
countMines = foldr (\x acc -> if '*' == x then acc + 1 else acc) 0

annotate :: [String] -> [String]
annotate board =
  let
    eachAreaMines = ( map $
                      map (countMines . concat)
                      . transpose
                      . map mineWindow
                        -- ^ creating column groups by each row and count
                    )
                    . mineWindow $ -- creating row groups by row

                    board
  in
    zipWith
    ( zipWith (\count cell ->
                 case (count, cell) of
                   (_, '*') -> '*'
                   (0, _) -> ' '
                   (_, _) -> intToDigit count
              )
    ) eachAreaMines board
