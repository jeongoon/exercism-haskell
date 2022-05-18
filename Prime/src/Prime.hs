module Prime (nth) where
type MyInt = Integer
primesTME :: [MyInt]
primesTME =   2 : ([3,5..] `minus` foldt [ [p*p,p*p+2*p..] | p <- primes_ ])
  where
    primes_ = 3 : ([5,7..] `minus` foldt [ [p*p,p*p+2*p..] | p <- primes_ ])
    foldt ~((x:xs):t) = x : union xs (foldt (pairs t))
    pairs ~((x:xs):ys:t) = (x : union xs ys) : pairs t
minus :: [MyInt] -> [MyInt] -> [MyInt]
minus xs@(x:xt) ys@(y:yt) = case compare x y of
                              LT -> x : minus xt ys
                              EQ ->     minus xt yt
                              GT ->     minus xs yt
minus a b = a
union :: [MyInt] -> [MyInt] -> [MyInt]
union xs@(x:xt) ys@(y:yt) = case compare x y of
                              LT -> x : union xt ys
                              EQ -> x : union xt yt
                              GT -> y : union xs yt
nth :: Int -> Maybe Integer
nth n
  | n < 1 = Nothing
  | otherwise = Just $ primesTME !! (n - 1)

-- all primes > 5 are in the form of 6n+1 or 6n+5 (for n >= 1)
primes :: [Integer]
primes = 2 : 3 : 5 : filter (go $ drop 2 primes) candidates
  where
    candidates = [x | n6 <- [6, 12 ..], x <- [n6 + 1, n6 + 5]]
    go (p:ps) n = case n `quotRem` p of
      (_, 0) -> False
      (q, _) -> q < p || go ps n
    go _ _ = undefined

nth' :: Int -> Maybe Integer
nth' n
  | n < 1 = Nothing
  | otherwise = Just . (primes !!) . pred $ n
