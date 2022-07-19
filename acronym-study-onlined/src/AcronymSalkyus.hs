module AcronymSalkyus (abbreviate) where
  import Data.Char

  abbreviate :: String -> String
  abbreviate xs = map (toUpper . head) $
                  concatMap ((splitWhen isLower . toTitleCase) . (takeWhile isLetter . dropWhile (not . isLetter))) $
                  concatMap (splitWhen (=='-')) $ 
                  words xs

  splitWhen :: (Char -> Bool) -> [Char] -> [[Char]]
  splitWhen p s =  case dropWhile p s of
                        "" -> []
                        s' -> w : splitWhen p s''
                              where (w, s'') = break p s'
                              
  toTitleCase :: [Char] -> [Char]
  toTitleCase [] = []
  toTitleCase xss@(x:xs) = if isUpper x then xss else toTitle x : xs
