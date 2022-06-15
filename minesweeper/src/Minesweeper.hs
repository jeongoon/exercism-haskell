module Minesweeper (minesweeperRange, annotate) where

minesweeperRange :: [a] -> [[a]]
minesweeperRange ms =
  let
    minesweeperRange' acc ms =
      let
        ks = take 3 ms
        rest = drop (length ks) ms
      in
        if 
        

--annotate :: [String] -> [String]
annotate board =
  let
    rowGroups = minesweeperRange board
  in
    rowGroups
