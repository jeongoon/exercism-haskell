module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA = mapM eachRNA
  where
    eachRNA = \x ->
                case x of
                  'G' -> pure 'C'
                  'C' -> pure 'G'
                  'T' -> pure 'A'
                  'A' -> pure 'U'
                  _   -> Left x
