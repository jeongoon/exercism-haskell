module Queens (boardString, canAttack) where

-- | takes two possible queen coordinate and create a board

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black =
  snd . foldr createBoardWith (queenIndexSymbolPairs, []) $ [0..63]

  where
    createBoardWith =
      \x (qs, acc) ->
        let
          (queenExists, symbol, qs') = takeQueen x qs
          acc'
            | (x+1) `rem` 8 == 0  = '\n': acc
            | otherwise           = ' ' : acc

        in
          if queenExists then (qs', symbol : acc')
          else                (qs',    '_' : acc')

    queenIndexSymbolPairs =
      foldr genIndexWithSymbols [] [(white, 'W'), (black, 'B')]
      where
        genIndexWithSymbols =
          \(mbCoord, sym) acc ->
            case mbCoord of
              Just (r,c) ->
                -- make index from coord with symbol as tuple
                (r * 8 + c, sym) : acc
              Nothing ->
                acc

    -- | check c(a coordinate) is found in qCoordsWithSymbol
    takeQueen c qIndexWithSymbols =
      takeQueen' [] qIndexWithSymbols
      where
        takeQueen' checked [] = (False, 'E', checked)
        takeQueen' checked (qos@(qcoord, sym):qoss)
          | qcoord == c = (True, sym, checked ++ qoss)
          | otherwise =
              takeQueen' (qos:checked) qoss

-- | check a queen is able to attack to another
canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack _queenA@(ar, ac) _queenB@(br, bc) =
  (ar == br || ac == bc || abs (ar-br) == abs (ac-bc))
