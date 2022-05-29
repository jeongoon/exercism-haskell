module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map)
import qualified Data.Map as M

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

-- pointer free version
nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts =
  \str ->
    mapM ( \x ->
             case x of
               'A' -> pure A
               'C' -> pure C
               'G' -> pure G
               'T' -> pure T
               _   -> error $ '`' : x : "` is invalid nucleotide code."
         ) str
  >>=
  return .
  (foldr (flip (M.insertWith (+)) 1) M.empty)
