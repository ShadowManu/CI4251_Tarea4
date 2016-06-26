module Queue
( newQueue
, increaseQueue
, decreaseQueue
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Monad (replicateM_)
import Test.QuickCheck (generate)

import Person
import Locks

-- DATA TYPES

type Queue = Chan Person

---- QUEUE FUNCTIONS

initSizeQueue :: Int
initSizeQueue = 10

newQueue :: IO Queue
newQueue = do
  queue <- newChan
  replicateM_ initSizeQueue $ increaseQueue queue
  return queue

increaseQueue :: Queue -> IO ()
increaseQueue q = do
  pers <- generate genPerson
  case pers of
    -- Clean personnel is abusive
    p@(Person Clean _) -> unGetChan q p
    p                  -> writeChan q p

decreaseQueue :: Queue -> Locks -> IO ()
decreaseQueue q l = do
  person <- readChan q

  let process (Person _ action) = forkIO $ action l
  _ <- process person

  increaseQueue q
  decreaseQueue q l
