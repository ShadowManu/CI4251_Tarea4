module Locks
( Locks(Locks)
, enter
, number
, available
, newLocks
) where

import Control.Concurrent.MVar (MVar, newMVar)

---- DATA TYPES

data Locks = Locks
  { enter :: MVar ()
  , number :: MVar Int
  , available :: MVar () }

---- LOCK FUNCTIONS

newLocks :: IO Locks
newLocks = do
  e <- newMVar ()
  n <- newMVar 0
  a<- newMVar ()

  return Locks { enter = e, number = n, available = a }
