module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Control.Arrow
import Control.Concurrent.STM
import Control.Monad            ((>=>))
import Data.Maybe               (fromJust)

type NodeR           v = TVar  (Node v)
type MaybeNodeR      v = Maybe (NodeR v)

type NodeRPair       v = (NodeR v,      NodeR v)
type MaybeNodeRPair  v = (MaybeNodeR v, MaybeNodeR v)

data Node v = Node { _prevMaybeNodeR :: MaybeNodeR v
                   , _nextMaybeNodeR :: MaybeNodeR v
                   , _nodeValue :: v
                   }

-- | wrapper data type for deque
type DequeInner v = Either (MaybeNodeR v) (NodeRPair v)
type Deque      v = TVar (DequeInner v)

-- | create Deque
mkDeque :: IO (Deque v)
mkDeque = newTVarIO (Left Nothing)

-- | add a new node at the front
unshift, push :: Deque v -> v -> IO ()
unshift dq v = atomically $
  runKleisli
  ( Kleisli readTVar
    >>> ( ( mbSingletone v >>> right (unshift' &&& returnA) )
          ||| (first unshift' >>^ Right) )
    >>> Kleisli (writeTVar dq) ) dq

  where unshift' = unshift'New Node first v

push dq v = atomically $
  runKleisli
  ( Kleisli readTVar
    >>> ( ( mbSingletone v >>> right (returnA &&& push') )
          ||| (second push' >>^ Right) )
    >>> Kleisli (writeTVar dq) ) dq

  where push' = unshift'New (flip Node) second v

-- | add a new node at the back

singleNode :: v -> Node v
singleNode =  Node Nothing Nothing
{-# INLINE singleNode #-}

mbSingletone :: v -> Kleisli STM (MaybeNodeR v)
                     (Either (MaybeNodeR v) (NodeR v))
mbSingletone = \v -> Kleisli $
  maybe ((singleNode ^>> newTVar >=> return . Left . Just) v)
        (Right ^>> return)
{-# INLINE mbSingletone #-}

unshift'New
  :: (MaybeNodeR v -> MaybeNodeR v -> v -> (Node v)) -> -- Node Constructor
     ((b -> MaybeNodeR v) -> (MaybeNodeRPair v) -> (MaybeNodeRPair v)) ->
     v ->
     Kleisli STM (NodeR v) (NodeR v)

unshift'New =
  \mkNodeData -> \first' -> \v ->
    Kleisli $
    \former -> do
      newBoundaryNode <- newTVar $ mkNodeData Nothing (Just former) v

      modifyTVar former $
        \(Node p n v') -> -- note: p, n :: MaybeNodeR v
          (uncurry Node)
          (first' (arr (const $ Just newBoundaryNode)) $ (p,n)) v'

      return newBoundaryNode
{-# INLINE unshift'New #-}


pop, shift :: Deque v -> IO (Maybe v)

-- | remove a node at the front and return its value
shift dq =
  atomically $ runKleisli
  ( Kleisli readTVar >>>
    ( Kleisli (maybe (return Nothing) (fmap Just . shiftLastV dq))
      ||| ( first (Kleisli (shiftVPlus _nextMaybeNodeR))
            >>> arr (uncurry cleanDequeData)
            >>> second (Kleisli (writeTVar dq))
            >>> arr (Just . fst)
          )
      )
  ) dq

-- | remove a node at the back return its value
pop dq =
  atomically $ runKleisli
  ( Kleisli readTVar >>>
    ( Kleisli (maybe (return Nothing) (fmap Just . shiftLastV dq))
      ||| ( second (Kleisli (shiftVPlus _prevMaybeNodeR))
            >>> arr ((uncurry . flip) cleanDequeData)
            >>> second (Kleisli (writeTVar dq))
            >>> arr (Just . fst)
          )
      )
  ) dq

shiftLastV :: Deque v -> (NodeR v -> STM v)
shiftLastV = \dq ->
  readTVar >=> (return . _nodeValue) >=>
  \v -> writeTVar dq (Left Nothing) >> -- empty deque
  return v
{-# INLINE shiftLastV #-}

shiftVPlus :: ( Node v -> (MaybeNodeR v) ) ->
              NodeR v -> STM (MaybeNodeR v, v)
shiftVPlus = -- Plus means there is an extra datum
  \left' -> readTVar >=> return . (arr left' &&& arr _nodeValue)
{-# INLINE shiftVPlus #-}

cleanDequeData :: (MaybeNodeR v, v) -> NodeR v -> (v, DequeInner v)
cleanDequeData = \(mbl', v) r' ->
  let l' = fromJust mbl' in  ( v, if l' == r'
                                  then Left (Just l')
                                  else Right (l', r')
                             )

{-# INLINE cleanDequeData #-}
