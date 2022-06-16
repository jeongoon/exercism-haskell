module Minesweeper (minesweeperRange, annotate) where

minesweeperRange :: [a] -> [[a]]
minesweeperRange =
  let (acc, lastRange) = foldl
      (\(acc, prev) m ->
         let
           new =
             if length prev < 3 then
               prev ++ [m]
             else
               (tail prev) ++ [m]
         in
           (new:acc, new)
        ) ([take 2 ms], (drop 2 ms))
  in
    (reverse acc) ++ [


--annotate :: [String] -> [String]
annotate board =
  let
    rowGroups = minesweeperRange board
  in
    rowGroups
