module IORefQ

        

          

  (Deque
  ,mkDeque

        

          

  , push

        

          

  , pop

        

          

  , shift

        

          

  , unshift

        

          

  ) where

        

          


        

          

import Control.Applicative

        

          

import Control.Arrow

        

          

--import Control.Concurrent.STM
import Data.IORef

        

          

import Control.Monad

        

          


        

          

-- Test require mutable state..

        

          

-- Invariants:

        

          

-- 1. left is Nothing <=> right is Nothing

        

          

data Element a = Element 

        

          

      { elementLeft  :: IORef (Maybe (Element a))

        

          

      , _elementValue :: a 

        

          

      , elementRight :: IORef (Maybe (Element a))

        

          

      }

        

          


        

          

singE :: a -> IO (Element a)

        

          

singE x = Element <$> newIORef Nothing <*> pure x <*> newIORef Nothing

        

          


        

          

type Storage a = (Element a,Element a)

        

          

data Deque a = Deque (IORef (Maybe (Element a, Element a)))

        

          


        

          

mkDeque :: IO (Deque a)

        

          

mkDeque = Deque <$> newIORef Nothing

        

          


        

          

push :: Deque a -> a -> IO ()

        

          

push d x = insertElement d x insert

        

          

  where

        

          

    insert = runKleisli (second (Kleisli $ boundaryElement rear x))

        

          


        

          

unshift :: Deque a -> a -> IO ()

        

          

unshift d x = insertElement d x insert

        

          

  where

        

          

    insert = runKleisli (first (Kleisli $ boundaryElement front x))

        

          


        

          

rear, front :: (Element a -> IORef (Maybe (Element a)), Element a -> IORef (Maybe (Element a)))

        

          

rear = (elementLeft, elementRight)

        

          

front = (elementRight, elementLeft)

        

          


        

          

boundaryElement :: (Element a -> IORef (Maybe (Element a)), Element a -> IORef (Maybe (Element a))) -> a -> Element a -> IO (Element a)

        

          

boundaryElement (f,g) x e = do

        

          

   n <- singE x -- create new element

        

          

   writeIORef (f n) (Just e)

        

          

   writeIORef (g e) (Just n)

        

          

   return n

        

          

-- {- INLINE boundaryElement -}

        

          


        

          

insertElement :: Deque a -> a -> (Storage a -> IO (Storage a)) -> IO ()

        

          

insertElement (Deque s) x insert =

        

          

  maybe new (writeIORef s . Just <=< insert) =<< readIORef s

        

          

  where new = singE x >>= \e -> writeIORef s $ Just (e,e)

        

          


        

          

pop :: Deque a -> IO (Maybe a)

        

          

pop (Deque s) = maybe (return Nothing) get =<< readIORef s

        

          

  where

        

          

    get (l,Element rl rx _) = do

        

          

      writeIORef s =<< return . (fmap (\r' -> (l,r'))) =<< readIORef rl

        

          

      return $ Just rx

        

          


        

          

shift :: Deque a -> IO (Maybe a)

        

          

shift (Deque s) = maybe (return Nothing) get =<< readIORef s

        

          

    where

        

          

      get (Element _ lx lr, r) = do

        

          

        writeIORef s =<< return . (fmap (\l' -> (l',r))) =<< readIORef lr

        

          

        return $ Just lx
