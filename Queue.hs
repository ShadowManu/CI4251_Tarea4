{- |
Module: $Header$
Copyright: (c) Manuel Pacheco 2016
License: MIT
Maintainer: manuelalejandropm@gmail.com
Queueing and classic syncing operations for a bathroom implementation
-}

module Queue
( runClassic
) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan

import Control.Monad (replicateM_, forever)
import Test.QuickCheck (generate)

import Person
import Lock

---- CONSTANTS

qSize :: Int
qSize = 5

---- DATA TYPES

type Queue = Chan Person

---- QUEUE FUNCTIONS

-- Creates a new queue (with a broadcast duplicate) with an qSize initial load of random persons
newQs :: IO (Queue, Queue)
newQs = do
  q <- newChan
  dup <- dupChan q
  replicateM_ qSize $ addQ q
  return (q, dup)

-- Adds a new random person to the queue following the queue rules
addQ :: Queue -> IO ()
addQ q = do
  pers <- generate genPerson
  case pers of
    p@(Person Clean _ _) -> unGetChan q p -- Clean personnel is abusive
    p                    -> writeChan q p

-- Pops the next person from the queue, process it, and add a new one to the queue
stepQ :: Queue -> Lock -> IO ()
stepQ q l = do
  p <- readChan q
  processPerson l p
  addQ q

-- Process a person trying to enter the bathroom, blocking if not possible,
-- sending the activity over a new thread when ready
processPerson :: Lock -> Person -> IO ()
processPerson l p = do
  askL (genre p) l
  _ <- forkIO $ do
    action p
    dropL l
  return ()

-- Given a report channel and a lock, report the status of the bathroom
report :: Queue -> Lock -> IO ()
report rep lock = forever $ do
  (gen, num) <- infoL lock
  putStrLn $ pretty gen num

  ----------------------------------------------------------------
  -- TEACHER NOTE: the queue (Chan) state should be reported here,
  -- but I couldn't make it work properly.

  threadDelay 300000 -- 0.3 Seconds

  where
    pretty Nothing _ = "No hay nadie ahorita."
    pretty (Just Man) 1 = "Hay " ++ "1" ++ " hombre  actualmente."
    pretty (Just Man) num = "Hay " ++ show num ++ " hombres actualmente."
    pretty (Just Woman) 1 = "Hay " ++ "1" ++ " mujer   actualmente."
    pretty (Just Woman) num = "Hay " ++ show num ++ " mujeres actualmente."
    pretty (Just Clean) _ = "Ahorita se encuentra el personal de limpieza."

runClassic :: IO ()
runClassic = do
  putStrLn "VERSION UTILIZANDO MVAR Y TCHAN"

  (queue, dup) <- newQs
  lock <- newL

  _ <- forkIO $ report dup lock
  forever $ stepQ queue lock
