module Person
( Genre(Man, Woman, Clean)
, Person(Person)
, genPerson
) where

import Control.Concurrent
import Test.QuickCheck

import Locks

---- Data Types

data Genre = Man | Woman | Clean deriving Show
data Person = Person Genre (Locks -> IO ())
type Time = Int

---- HELPER FUNCTIONS

-- Helper function to represent seconds as microseconds
asMicro :: Int -> Int
asMicro n = n * (10 ^ (6 :: Int))

---- PERSON ACTIONS

manIO :: Locks -> Time -> IO ()
manIO lock time = do
  _ <- takeMVar $ enter lock -- Get ready to enter
  n <- takeMVar $ number lock -- Check the number of people inside
  putStrLn $ "I've found " ++ show n ++ "persons inside!"-- TODO DEBUGGING

  if n < 3
    then -- If room is available
      putMVar (number lock) (n+1) -- Say you're entering

    else do -- If not, wait for availability
      putMVar (number lock) n -- Put the number back
      takeMVar $ available lock -- Wait for someone to leave
      modifyMVar_ (number lock) $ \num -> return $ num+1 -- Say you're entering

  putMVar (enter lock) () -- Let others get ready to enter
  threadDelay time -- Clean your face and flush your bladder

  modifyMVar_ (number lock) $ \num -> return $ num-1 -- Say you're leaving
  _ <- tryPutMVar (available lock) ()-- Also let them know a slot is available

  putStrLn "I FINISHED BITCHES!"-- TODO JUST DEBUGGING
  return ()

---- GENERATOR FUNCTIONS

-- Generator for the person genre with problem-set probablities
genGenre :: Gen Genre
genGenre = frequency [(49, return Man), (49, return Woman), (2, return Clean)]

-- Generator for the random time that a person stays in the bathroom, in microseconds
-- Range chosen is between 1 and 3 seconds
genTime :: Gen Time
genTime = choose (asMicro 1, asMicro 3)

-- Generator for the people entering the bathroom
genPerson :: Gen Person
genPerson = do
  genre <- genGenre
  time <- genTime
  return $ Person genre $ \lock -> manIO lock time
