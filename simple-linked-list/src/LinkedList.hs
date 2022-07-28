module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

data LinkedList a = Nil | LinkedList { datum :: a
                                     , next  :: LinkedList a
                                     } deriving (Eq, Show)

nil :: LinkedList a
nil = Nil

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _   = False

new :: a -> LinkedList a -> LinkedList a
new = LinkedList

fromList :: [a] -> LinkedList a
fromList = foldr new Nil

toList :: LinkedList a -> [a]
toList Nil = []
toList (LinkedList x nextLinkedList) = x : toList nextLinkedList

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = foldl (flip new) Nil . toList

-- no need to implement
--datum :: LinkedList a -> a
--next :: LinkedList a -> LinkedList a
