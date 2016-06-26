module Lock
( Lock(Lock)
, newLock
, askLock
, dropLock
) where

import Control.Concurrent.MVar

import Person (Genre)

---- CONSTANTS

top :: Int
top = 3

---- DATA TYPES

data Lock = Lock
  { genre :: MVar (Maybe Genre)
  , number :: MVar Int
  , free :: MVar ()
  , empty :: MVar () }

---- LOCK FUNCTIONS

-- Creates a new lock structure
newLock :: IO Lock
newLock = do
  g <- newMVar Nothing
  n <- newMVar 0
  f <- newMVar ()
  e <- newMVar ()
  return $ Lock g n f e

-- Process the asking for a lock
askLock :: Genre -> Lock -> IO ()
askLock gen lock = do

  jg <- takeMVar (genre lock) -- Get genre
  case jg of

    Nothing -> do -- New Case, enter
      modifyMVar_ (number lock) (return . const 1) -- Increase the lock
      putMVar (genre lock) (Just gen) -- Put the new genre

    Just g -> if g == gen -- Existing case

      then do -- Same subcase
        num <- takeMVar (number lock) -- Get number of people

        if num < top -- Check if there is available space
          then do -- When available, enter
            putMVar (number lock) (num+1) -- Increase the number
            putMVar (genre lock) jg -- Put the genre back

          else do -- When not available, drop locks, wait and enter
            putMVar (number lock) num -- Drop the number
            putMVar (genre lock) jg -- Drop the genre
            takeMVar (free lock) -- Wait for free
            modifyMVar_ (number lock) (return . (+1)) -- Increase the number

      else do -- Different genre subcase, drop locks, wait and enter
        putMVar (genre lock) jg-- Drop the genre
        takeMVar (empty lock) -- Wait for empty
        modifyMVar_ (number lock) (return . (+1)) -- Put the number
        modifyMVar_ (genre lock) (return . const (Just gen)) -- Put the new genre

-- Process the returning of a lock
dropLock :: Lock -> IO ()
dropLock lock = do

  jg <- takeMVar (genre lock) -- Get genre
  num <- takeMVar (number lock) -- Get number


  if num == 1
    then do -- Emptying Case: Clean up, notify and leave
      _ <- tryTakeMVar (free lock) -- Clean free
      putMVar (number lock) 0 -- Clean number
      putMVar (genre lock) Nothing -- Clean genre
      _ <- tryPutMVar (empty lock) ()-- Notify empty
      return ()

    else if num == top
      then do -- Freeing case: Lower, notify and leave
        putMVar (number lock) (num-1)-- Lower number
        putMVar (genre lock) jg-- Put back genre
        _ <- tryPutMVar (free lock) () -- Notify free
        return ()

      else do -- Common case: Lower and leave
        putMVar (number lock) (num-1)-- Lower number
        putMVar (genre lock) jg-- Put back genre
