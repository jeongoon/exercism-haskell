module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Control.Monad (when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

data Node a = Init | Node { nodeValue :: (Maybe a)
                          , prevRef   :: IORef (Node a)
                          , nextRef   :: IORef (Node a)
                          }
data Deque a = Deque
  { _originRef :: IORef (Node a)
  }

-- | type for data accessor prevRef, nextRef
type RefAccessor a = (Node a) -> IORef (Node a)

-- | update field for preRef or nextRef
type UpdateField a = (Node a) -> IORef (Node a) -> (Node a)

type TargetAccessor a = IORef (Node a) -> IO (IORef (Node a))

-- | create Deque
mkDeque :: IO (Deque a)
mkDeque = do
      originRef <- newIORef Init
      writeIORef originRef (Node { nodeValue = Nothing
                                 , prevRef   = originRef
                                 , nextRef   = originRef
                                 }
                           )
      return $ Deque originRef

-- | some helper functions ...
updatePrevRef, updateNextRef :: Node a -> IORef (Node a) -> Node a
updatePrevRef node anIORef = node { prevRef = anIORef }
updateNextRef node anIoRef = node { nextRef = anIoRef }


-- | find target iterator (last or origin) and add new node front of it
pushFrontWith :: TargetAccessor a -> Deque a -> a -> IO ()
pushFrontWith targetRefIOAccessor dq newNodeValue = do
  let originRef =  _originRef dq

  -- find target node and a node before target
  targetRef     <- targetRefIOAccessor originRef
  targetNode    <- readIORef           targetRef
  let priorRef  =  prevRef             targetNode

  -- make a new node
  newRef <- newIORef Node { nodeValue = Just newNodeValue
                          , prevRef   = priorRef
                          , nextRef   = targetRef
                          }

  -- re connect prior target and target
  writeIORef targetRef $ updatePrevRef targetNode newRef
  priorNode <- readIORef priorRef -- this must be conducted after previous code
  writeIORef priorRef  $ updateNextRef priorNode  newRef

push, unshift :: Deque a -> a -> IO ()
push    = pushFrontWith
          (return :: TargetAccessor a) -- push in front of origin

unshift = pushFrontWith $
          (fmap prevRef) <$> readIORef -- push in front of first element


-- | find end point and remove from the deque and return it as a node
removeEnd :: RefAccessor a ->
             UpdateField a -> UpdateField a ->
             Deque a -> IO (Maybe a)

removeEnd prevRefAccessor updateNewEndNode updateOriginNode dq = do
  let originRef = _originRef dq
  originNode <- readIORef originRef
  let endRef = (prevRefAccessor originNode)
  currEndNode <- readIORef endRef

  when (endRef /= originRef) $ do -- not empty list
    let endRef' = prevRefAccessor currEndNode
    endNode' <- readIORef endRef'

    -- connect new end to origin
    writeIORef endRef'   $ updateNewEndNode endNode'   originRef
    writeIORef originRef $ updateOriginNode originNode endRef'

  -- return end node
  -- it is safe when end node == origin because origin node value is Nothing
  return $ nodeValue currEndNode

pop, shift :: Deque a -> IO (Maybe a)
pop   = removeEnd prevRef updateNextRef updatePrevRef
shift = removeEnd nextRef updatePrevRef updateNextRef
